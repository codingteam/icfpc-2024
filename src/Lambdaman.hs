{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}

module Lambdaman where

import Control.Monad
import Control.Monad.State
import Data.Array.IArray as A
import GHC.Generics
import Data.Hashable
import Data.Maybe
import Data.List (findIndex, elemIndex)
import qualified Data.HashSet as H
import qualified Data.PQueue.Prio.Min as Q

data Cell = Empty | Pill | Wall | Man
    deriving (Eq, Show, Generic)

instance Hashable Cell

data Direction = U | R | D | L
    deriving (Eq, Show)

type Position = (Int, Int)
type Grid = Array Position Cell

data Problem = Problem {
    pGrid :: !Grid
  , pPosition :: !Position
  , pOrigin :: !Position
  }
  deriving (Eq, Show, Generic)

instance Hashable (Array (Int,Int) Cell) where
    hashWithSalt salt a = hashWithSalt salt $ elems a

instance Hashable Problem

type Value = Int

data Path = Path {
      ptSteps :: ![Direction]
    , ptState :: !Problem
    }

instance Show Path where
    show p = concatMap show $ reverse $ ptSteps p

data AState = AState {
      aOpen :: !(Q.MinPQueue Int Path)
    , aClosed :: !(H.HashSet Problem)
    }

emptyAState :: AState
emptyAState = AState Q.empty H.empty

type A a = StateT AState IO a

calcStep :: Direction -> Position -> Position
calcStep U (y,x) = (y-1, x)
calcStep R (y,x) = (y, x+1)
calcStep D (y,x) = (y+1, x)
calcStep L (y,x) = (y, x-1)

evalStep :: (Direction, Problem) -> Problem
evalStep (step, p) =
    let pos' = calcStep step (pPosition p)
    in case pGrid p !? pos' of
        Nothing -> p
        Just Wall -> p
        _ -> p {pGrid = pGrid p // [(pos', Empty)], pPosition = pos'}

evalPath :: Problem -> Path -> Problem
evalPath p path =
    case ptSteps path of
        [] -> p
        (step : steps) ->
            let p' = evalStep (step, p)
                path' = path {ptSteps = steps}
            in  evalPath p' (appendPath step p' path')

isGoal :: Grid -> Bool
isGoal g = not $ any (== Pill) $ A.elems g

successors :: Problem -> [(Direction, Problem)]
successors p = mapMaybe check [U, R, D, L]
    where
        check step =
            let pos' = calcStep step (pPosition p)
            in  case pGrid p !? pos' of
                    Just Pill -> Just (step, evalStep (step, p))
                    Just Empty -> Just (step, evalStep (step, p))
                    _ -> Nothing

calcPriority' :: Path -> (Value, Value)
calcPriority' path = (originToCurrent, currentToGoal)
    where
        originToCurrent = length $ ptSteps path
        currentToGoal = length $ filter (== Pill) $ A.elems (pGrid $ ptState path)

appendPath :: Direction -> Problem -> Path -> Path
appendPath step p path =
    path {ptState = p, ptSteps = step : ptSteps path}

singletonPath :: Problem -> Path
singletonPath p = Path {ptState = p, ptSteps = []}

extractPath :: Path -> Problem
extractPath path = ptState path

aStar :: Problem -> A (Maybe Path)
aStar p = do
        let queue = Q.singleton 0 $ singletonPath p
        put $ AState queue H.empty
        loop
    where
        loop = do
            open <- gets aOpen
            case Q.minView open of
                Nothing -> return Nothing
                Just (path, rest) -> do
                    -- liftIO $ putStrLn $ "From queue: " ++ show path
                    modify $ \st -> st {aOpen = rest}
                    let grid = extractPath path
                    closed <- gets aClosed
                    if grid `H.member` closed
                      then do
                        -- liftIO $ putStrLn "we were here already, skip"
                        loop
                      else if isGoal $ pGrid grid
                             then return $ Just path
                             else do
                               modify $ \st -> st {aClosed = H.insert grid (aClosed st)}
                               forM_ (successors grid) $ \(step, y) -> do
                                 let path' = appendPath step y path
                                 let (priority1, priority2) = calcPriority' path'
                                 -- liftIO $ putStrLn $ "Check: " ++ show path'
                                 -- liftIO $ putStr $ showProblem $ ptState path'
                                 -- liftIO $ putStrLn $ "Priority: " ++ show (priority1, priority2)
                                 let priority = priority1 + priority2
                                 modify $ \st -> st {aOpen = Q.insert priority path' (aOpen st)}
                               loop

evalAStar :: Problem -> IO (Maybe Path)
evalAStar p = evalStateT (aStar p) emptyAState

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n lst = take n lst : chunksOf n (drop n lst)

decodeProblem :: [String] -> Problem
decodeProblem lines =
    let Just originY = findIndex ('L' `elem`) lines
        Just originX = elemIndex 'L' $ lines !! originY
        sizeY = length lines
        sizeX = length (head lines)

        decodeChar ' ' = Empty
        decodeChar 'L' = Empty
        decodeChar '#' = Wall
        decodeChar '.' = Pill

        lines' = concatMap (map decodeChar) lines
        indices :: [Position]
        indices = [(y, x) | y <- [0..sizeY-1], x <- [0..sizeX-1]]
        grid :: Grid
        grid = A.array ((0,0), (sizeY-1, sizeX-1)) $ zip indices lines'
        origin = (originY, originX)
    in  Problem grid origin origin

showProblem :: Problem -> String
showProblem p =
    let textGrid = fmap showCell (pGrid p)
        showCell Empty = ' '
        showCell Wall = '#'
        showCell Pill = '.'
        textGrid' = textGrid // [(pPosition p, 'L'), (pOrigin p, 'o')]
        (_, (maxY, maxX)) = A.bounds (pGrid p)
    in  unlines $ chunksOf (maxX+1) $ A.elems textGrid'

problemFromFile :: FilePath -> IO Problem
problemFromFile path = do
    text <- readFile path
    return $ decodeProblem $ lines text
