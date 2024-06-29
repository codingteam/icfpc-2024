{-# LANGUAGE FlexibleInstances, DeriveGeneric, BangPatterns, FlexibleContexts #-}

module Lambdaman (
    problemFromFile
,   evalAStar
,   greedySolve
,   showPath
) where

import Control.Monad
import Control.Monad.State
import Data.Array.Unboxed as U
import Data.Array.IArray as A
import GHC.Generics
import Data.Hashable
import Data.Maybe
import Data.Ord (comparing)
import Data.List (findIndex, elemIndex, minimumBy)
import qualified Data.HashSet as H
import qualified Data.PQueue.Prio.Min as Q
import Data.Word

-- Empty = 0
-- Pill = 1
-- Wall = 2
type Cell = Word8

emptyCell, pillCell, wallCell :: Cell
emptyCell = 0
pillCell = 1
wallCell = 2

-- instance Hashable Cell

data Direction = U | R | D | L
    deriving (Eq, Show)

type Position = (Int, Int)
newtype Grid = Grid { gGrid :: UArray Position Cell }
    deriving (Eq, Show)
type Grid16 = UArray Position Word16

data Problem = Problem {
    pGrid :: Grid
  , pPosition :: !Position
  , pOrigin :: !Position
  , pNPills :: !Value
  }
  deriving (Eq, Show, Generic)

findGridIndices :: (Eq a, IArray UArray a) => a -> UArray Position a -> [Position]
findGridIndices c grid = map fst $ filter (\(_i, e) -> e == c) $ U.assocs grid

instance Hashable Grid where
    hashWithSalt salt (Grid a) = foldr (flip hashWithSalt) salt $ findGridIndices pillCell a

instance Hashable Problem where
    hashWithSalt salt p =
        hashWithSalt (hashWithSalt salt $ pGrid p) (pPosition p)

type Value = Int

data Path = Path {
      ptSteps :: ![Direction]
    , ptOriginal :: Problem
    }

showPath :: [Direction] -> String
showPath steps = concatMap show $ steps

instance Show Path where
    show p = concatMap show $ reverse $ ptSteps p

data ProblemSet = ProblemSet {
      psPositions :: !(H.HashSet Position)
    , psNPills :: !(H.HashSet Value)
    , psGrids :: H.HashSet (Position, Grid)
  }

-- type ProblemSet = H.HashSet Problem

emptyPS :: ProblemSet
emptyPS = ProblemSet H.empty H.empty H.empty
-- emptyPS = H.empty

insertPS :: Problem -> ProblemSet -> ProblemSet
-- insertPS = H.insert

insertPS p ps =
    ProblemSet {
        psPositions = H.insert (pPosition p) (psPositions ps),
        psNPills = H.insert (pNPills p) (psNPills ps),
        psGrids = H.insert (pPosition p, pGrid p) (psGrids ps)
    }


memberPS :: Problem -> ProblemSet -> Bool
-- memberPS = H.member

memberPS p ps =
    pNPills p `H.member` psNPills ps &&
    pPosition p `H.member` psPositions ps &&
    (pPosition p, pGrid p) `H.member` psGrids ps


data AState = AState {
      aOpen :: !(Q.MinPQueue Int Path)
    , aClosed :: !ProblemSet
    }

emptyAState :: AState
emptyAState = AState Q.empty emptyPS

type A a = StateT AState IO a

calcStep :: Direction -> Position -> Position
calcStep U (y,x) = (y-1, x)
calcStep R (y,x) = (y, x+1)
calcStep D (y,x) = (y+1, x)
calcStep L (y,x) = (y, x-1)

evalStep :: Direction -> Problem -> Problem
evalStep step !p =
    let pos' = calcStep step (pPosition p)
        grid = gGrid $ pGrid p
    in case grid !? pos' of
        Nothing -> p
        Just 2 -> p
        Just 1 -> p {pGrid = Grid $ grid // [(pos', emptyCell)], pPosition = pos', pNPills = pNPills p - 1}
        Just 0 -> p {pGrid = Grid $ grid // [(pos', emptyCell)], pPosition = pos'}
        Just x -> error $ "BUG: impossible cell value " ++ show x

evalPath :: Path -> Problem
evalPath path = foldr evalStep (ptOriginal path) (ptSteps path)

isGoal :: Grid -> Bool
isGoal (Grid g) = not $ any (== pillCell) $ U.elems g

successors :: Problem -> [(Direction, Problem)]
successors p = mapMaybe check [U, R, D, L]
    where
        check step =
            let pos' = calcStep step (pPosition p)
            in  case gGrid (pGrid p) !? pos' of
                    Just 1 -> Just (step, evalStep step $! p)
                    Just 0 -> Just (step, evalStep step $! p)
                    _ -> Nothing

calcNPills :: Grid -> Value
calcNPills (Grid grid) = length $ filter (== pillCell) $ U.elems grid

calcPriority' :: Value -> Path -> (Value, Value)
calcPriority' distanceToPills path = (originToCurrent, currentToGoal)
    where
        originToCurrent = 0 -- length $ ptSteps path
        currentToGoal = pNPills (evalPath path) + distanceToPills

appendPath :: Direction -> Problem -> Path -> Path
appendPath step _p path =
    path {ptSteps = step : ptSteps path}

singletonPath :: Problem -> Path
singletonPath p = Path {ptOriginal = p, ptSteps = []}

extractPath :: Path -> Problem
extractPath path = evalPath path

type Mark = Word16

unchecked :: Mark
unchecked = maxBound

obstacle :: Mark
obstacle = maxBound-1

waveIteration :: Mark -> Grid16 -> Maybe [Position] -> Maybe (Grid16, [Position])
waveIteration startMark prevWaves mbIdxs =
    let startIdxs =
            case mbIdxs of
                Just idxs -> idxs
                Nothing -> findGridIndices startMark prevWaves
        nextIdxs = H.toList $ H.fromList $ concatMap checkNeighbours startIdxs
        waves' = prevWaves // [(i, startMark+1) | i <- nextIdxs]
        checkNeighbours i = mapMaybe check [U, R, D, L]
            where
                check dir =
                    let i' = calcStep dir i
                    in case prevWaves !? i' of
                        Just x | x == unchecked -> Just i'
                        _ -> Nothing
    in  if null startIdxs
            then Nothing
            else Just (waves', nextIdxs)

hasUnchecked :: Grid16 -> Bool
hasUnchecked grid = not $ null $ findGridIndices unchecked grid

initWave :: Grid -> Cell -> Grid16
initWave (Grid grid) start =
    let bs = bounds grid
        waves = A.array bs [(i, translate $ grid A.! i) | i <- A.indices grid]
        startMark = 0
        translate c
            | c == wallCell = obstacle
            | c == start = startMark
            | otherwise = unchecked
    in  waves

makeWave :: Grid -> Cell -> Grid16
makeWave grid start =
    let initialWaves = initWave grid start
        loop mark mbIdxs waves =
            case waveIteration mark waves mbIdxs of
                Just (waves', nextIdxs) -> loop (mark+1) (Just nextIdxs) waves'
                Nothing -> waves
    in  loop 0 Nothing initialWaves

getDistance :: Position -> Direction -> Grid16 -> Value
getDistance pos dir distances =
    case distances !? calcStep dir pos of
        Nothing -> fromIntegral unchecked
        Just d -> fromIntegral d

greedyStep :: Bool -> Problem -> Maybe (Direction, Problem)
greedyStep checkNPills p =
    let distances = makeWave (pGrid p) pillCell
        alternatives = mapMaybe check [U, D, L, R]
        check dir =
            let pos' = calcStep dir (pPosition p)
            in  case gGrid (pGrid p) !? pos' of
                    Nothing -> Nothing
                    Just x | x == pillCell -> Just (dir, 0, evalStep dir p)
                           | x == wallCell -> Nothing
                           | otherwise -> Just (dir, distances A.! pos', evalStep dir p)
    in  if (checkNPills && pNPills p == 0) || null alternatives
            then Nothing
            else Just $ (\(dir,_,p') -> (dir, p')) $ minimumBy (comparing (\(_,d,_) -> d)) alternatives

greedySolve :: Problem -> [Direction]
greedySolve p =
    case greedyStep True p of
        Nothing -> []
        Just (step, p') -> step : greedySolve p'

hasNeighbourPills :: Problem -> Bool
hasNeighbourPills p = any check [U, L, D, R]
    where
        check dir =
            case gGrid (pGrid p) !? calcStep dir (pPosition p) of
                Just x | x == pillCell -> True
                _ -> False

greedyGoToPill :: Problem -> Path -> A (Maybe (Path, Problem))
greedyGoToPill p path = do
    if hasNeighbourPills p
        then return $ Just (path, p)
        else do
            case greedyStep False p of
                Nothing -> return Nothing
                Just (step, p') -> do
                    -- liftIO $ putStrLn $ "Greedy check: " ++ show step ++ ": " ++ show (pNPills p) ++ " -> " ++ show (pNPills p') ++ ", has neigbour pills => " ++ show (hasNeighbourPills p')
                    markClosed p'
                    let path' = appendPath step p' path
                    if pNPills p' < pNPills p && (pNPills p' == 0 || hasNeighbourPills p')
                        then do
                            -- liftIO $ putStrLn $ "Greedy returns: " ++ show path'
                            return $ Just (path', p')
                        else do
                            -- liftIO $ putStrLn "Greedy iteration"
                            greedyGoToPill p' path'

markClosed :: Problem -> A ()
markClosed grid =
    modify $ \st -> st {aClosed = insertPS grid (aClosed st)}

enqueue :: Value -> Path -> A ()
enqueue priority path = do
    modify $ \st -> st {aOpen = Q.insert priority path (aOpen st)}

aStar :: Problem -> A (Maybe Path)
aStar p = do
        let queue = Q.singleton 0 $ singletonPath p
        put $ AState queue emptyPS
        loop
    where
        loop = do
            open <- gets aOpen
            case Q.minView open of
                Nothing -> do
                    -- liftIO $ putStrLn "end of queue"
                    return Nothing
                Just (path, rest) -> do
                    -- liftIO $ putStrLn $ "From queue: " ++ show path
                    modify $ \st -> st {aOpen = rest}
                    let grid = extractPath path
                    closed <- gets aClosed
                    if grid `memberPS` closed
                      then do
                        -- liftIO $ putStrLn "we were here already, skip"
                        loop
                      else if isGoal $ pGrid grid
                             then return $ Just path
                             else do
                               markClosed grid
                               mbToPill <- greedyGoToPill grid path
                               case mbToPill of
                                 Nothing -> return Nothing
                                 Just (pathToPill, grid') -> do
                                   -- when (pPosition grid /= pPosition grid') $ do
                                       -- liftIO $ putStrLn $ "Greedy algorithm returned: " ++ show pathToPill
                                       -- liftIO $ putStr $ showProblem grid'
                                   if isGoal $ pGrid grid'
                                     then return $ Just pathToPill
                                     else do
                                       let distancesToPills = makeWave (pGrid grid') pillCell
                                       forM_ (successors grid') $ \(step, y) -> do
                                         let path' = appendPath step y pathToPill
                                         let distanceToPills = getDistance (pPosition grid') step distancesToPills
                                         let (priority1, priority2) = calcPriority' distanceToPills path'
                                         -- liftIO $ putStrLn $ "Check: " ++ show path' ++ ": " ++ show (pPosition grid') ++ " -> " ++ show (pPosition $ extractPath path')
                                         -- liftIO $ putStr $ showProblem $ evalPath path'
                                         -- liftIO $ putStrLn $ "Priority: " ++ show (priority1, priority2)
                                         let priority = priority1 + priority2
                                         enqueue priority path'
                                       loop

evalAStar :: Problem -> IO (Maybe Path)
evalAStar p = evalStateT (aStar p) emptyAState

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n lst = take n lst : chunksOf n (drop n lst)

decodeProblem :: [String] -> Problem
decodeProblem fileLines =
    let originY = fromJust $ findIndex ('L' `elem`) fileLines
        originX = fromJust $ elemIndex 'L' $ fileLines !! originY
        sizeY = length fileLines
        sizeX = length (head fileLines)

        decodeChar ' ' = emptyCell
        decodeChar 'L' = emptyCell
        decodeChar '#' = wallCell
        decodeChar '.' = pillCell
        decodeChar c = error $ "BUG: unknown lambdaman map character \"" ++ [c] ++ "\""

        lines' = concatMap (map decodeChar) fileLines
        cellIndices :: [Position]
        cellIndices = [(y, x) | y <- [0..sizeY-1], x <- [0..sizeX-1]]
        grid :: Grid
        grid = Grid $ U.array ((0,0), (sizeY-1, sizeX-1)) $ zip cellIndices lines'
        origin = (originY, originX)
        nPills = calcNPills grid
    in  Problem grid origin origin nPills

showProblem :: Problem -> String
showProblem p =
    let textGrid :: U.UArray Position Char
        textGrid =
            let idxs = U.indices $ gGrid (pGrid p)
            in U.array (A.bounds (gGrid $ pGrid p)) [(i, showCell ((gGrid $ pGrid p) U.! i)) | i <- idxs]
        showCell 0 = ' '
        showCell 2 = '#'
        showCell 1 = '.'
        showCell x = error $ "Unknown cell type " ++ show x
        textGrid' = textGrid // [(pPosition p, 'L'), (pOrigin p, 'o')]
        (_, (_maxY, maxX)) = U.bounds (gGrid $ pGrid p)
    in  unlines $ chunksOf (maxX+1) $ U.elems textGrid'

problemFromFile :: FilePath -> IO Problem
problemFromFile path = do
    text <- readFile path
    return $ decodeProblem $ lines text

