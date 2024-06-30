module Sim3D (
    simulate
,   simulateStep
,   parseBoard
,   Board
,   stateFromBoard
,   Sim3dState(..)
,   Sim3dError
,   Cell(..)
,   shiftBy
,   execSimulation
,   runSimulation
) where

import qualified Data.Char as C
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Either
import Data.List (transpose)

data Cell =
      Empty
    | Value Integer
    | MoveLeft
    | MoveRight
    | MoveUp
    | MoveDown
    | Plus
    | Minus
    | Multiply
    | Divide
    | Modulo
    | TimeWarp
    | Equal
    | NotEqual
    | OutputS
    | InputA
    | InputB
    deriving (Eq, Show)

type Position = (Integer, Integer)

data Board = Board {
    cells :: M.Map Position Cell,
    minCoords :: (Integer, Integer),
    maxCoords :: (Integer, Integer)
} deriving (Eq, Show)

getCell :: Board -> Position -> Cell
getCell board coords = M.findWithDefault Empty coords $ cells board

isNumber :: DT.Text -> Bool
isNumber text =
    case DT.unpack text of
        [] -> False
        ('-':d:igits) -> all C.isDigit (d:igits)
        digits -> all C.isDigit digits

readCell :: DT.Text -> Cell
readCell "." = Empty
readCell x | isNumber x = Value $ read $ DT.unpack x
readCell "<" = MoveLeft
readCell ">" = MoveRight
readCell "^" = MoveUp
readCell "v" = MoveDown
readCell "+" = Plus
readCell "-" = Minus
readCell "*" = Multiply
readCell "/" = Divide
readCell "%" = Modulo
readCell "@" = TimeWarp
readCell "=" = Equal
readCell "#" = NotEqual
readCell "S" = OutputS
readCell "A" = InputA
readCell "B" = InputB
readCell cell = error $ DT.unpack $ "Unknown cell type \"" <> cell <> "\""

boardFromList :: [[Cell]] -> Board
boardFromList cellsList =
    let indexedCells = concat $ zipWith (\y row -> zipWith (\x cell -> ((x, y), cell)) [0..] row) [0..] cellsList
        minX = minimum $ map (fst . fst) indexedCells
        minY = minimum $ map (snd . fst) indexedCells
        maxX = maximum $ map (fst . fst) indexedCells
        maxY = maximum $ map (snd . fst) indexedCells in
    Board (M.fromList indexedCells) (minX, minY) (maxX, maxY)

parseBoard :: DT.Text -> Board
parseBoard contents =
    let textRows = DT.lines contents
        textCells = map DT.words textRows in
    normalize $ boardFromList $ map (map readCell) textCells

readBoard :: String -> IO Board
readBoard path = do
    contents <- DTI.readFile path
    return $ parseBoard contents

type Inputs = (Integer, Integer)

replaceInputs :: Board -> Inputs -> Board
replaceInputs board (a, b) =
    let replaceCell InputA = Value a
        replaceCell InputB = Value b
        replaceCell cell = cell
    in board { cells = M.map replaceCell $ cells board }

printBoard :: Board -> IO ()
printBoard board =
    case evalSimulation showBoard (stateFromBoard board) of
        Left err -> DTI.putStrLn $ "Error: " <> err
        Right table -> DTI.putStrLn table

simulate :: String -> Inputs -> IO ()
simulate boardPath inputs = do
    board <- readBoard boardPath
    let board' = replaceInputs board inputs
        s3dState = stateFromBoard board'
        isAlreadyOver =
            let result = evalSimulation isGameOver s3dState
            in isLeft result || (result == Right True)
    if isAlreadyOver then do
        putStrLn "I cannot see the goal on the initial board. This game will never end."
        doSimulation s3dState False
    else
        doSimulation s3dState True

doSimulation :: Sim3dState -> Bool -> IO ()
doSimulation s3dState checkGameOver = do
    let board = s3dsCurBoard s3dState
    printBoard board
    let gameOver =
            let result = evalSimulation isGameOver s3dState
            in isLeft result || (result == Right True)
    if checkGameOver && gameOver then
        putStrLn "Game over!"
    else do
        putStrLn "Press enter to continue."
        _ <- getLine
        case execSimulation simulateStep s3dState of
            Left err -> DTI.putStrLn $ "The game ended: " <> err
            Right s -> doSimulation s checkGameOver

data Sim3dState = Sim3dState {
    s3dsCurBoard :: Board --- ^ Current state of the board (read-only)
,   s3dsNextBoard :: Board --- ^ Next state of the board (we're updating it)
,   s3dsTime :: Integer --- ^ Current time (starts at 1)
,   s3dsPreviousBoards :: [Board] --- ^ Stack of previous board states. The top (first list element) is the most recent state.
,   s3dsOutput :: Cell --- ^ The result of the computation. The simulation terminates when this field becomes non-`Empty`.
}

stateFromBoard :: Board -> Sim3dState
stateFromBoard board =
    Sim3dState {
        s3dsCurBoard = board
    ,   s3dsNextBoard = board
    ,   s3dsTime = 1
    ,   s3dsPreviousBoards = []
    ,   s3dsOutput = Empty
    }

type Sim3dError = DT.Text
type Sim3dM a = StateT Sim3dState (Except Sim3dError) a

execSimulation :: Sim3dM a -> Sim3dState -> Either Sim3dError Sim3dState
execSimulation action initialState =
    runExcept $ execStateT action initialState

evalSimulation :: Sim3dM a -> Sim3dState -> Either Sim3dError a
evalSimulation action initialState =
    runExcept $ evalStateT action initialState

runSimulation :: Sim3dM a -> Sim3dState -> Either Sim3dError (a, Sim3dState)
runSimulation action initialState =
    runExcept $ runStateT action initialState

isGameOver :: Sim3dM Bool
isGameOver = do
    output <- gets s3dsOutput
    pure $ output /= Empty

--- Runs a single tick of the simulation and returns current output.
simulateStep :: Sim3dM Cell
simulateStep = do
    updateCells
    moveNextToCurrent
    modify' $ \s -> s { s3dsTime = 1 + s3dsTime s }
    gets s3dsOutput

readAt :: Position -> Sim3dM Cell
readAt pos = do
    board <- gets s3dsCurBoard
    pure $ getCell board pos

readNextBoardAt :: Position -> Sim3dM Cell
readNextBoardAt pos = do
    board <- gets s3dsNextBoard
    pure $ getCell board pos

ensureWriteIsAllowed :: Position -> Cell -> Sim3dM ()
ensureWriteIsAllowed pos value = do
    currentValue <- readAt pos
    nextValue <- readNextBoardAt pos

    -- we can write into an output cell any time; its invariants are checked in `storeOutput`
    let isOutput = currentValue == OutputS
    -- we haven't written to this cell on this tick yet
    let isFirstWrite = currentValue == nextValue
    -- we're writing the same value that's already in the cell
    let overwriteWithSame = nextValue == value

    when (not isOutput && not isFirstWrite && not overwriteWithSame) $ do
        let fPos = DT.pack $ show pos
        let fValue = DT.pack $ show value
        let fNextValue = DT.pack $ show nextValue
        sim3dThrowError $
            "Error: trying to overwrite previously written value of \""
            <> fNextValue <> "\" with \"" <> fValue <> "\" at " <> fPos

writeTo :: Position -> Cell -> Sim3dM ()
writeTo pos value = do
    ensureWriteIsAllowed pos value
    currentValue <- readAt pos
    case currentValue of
        OutputS -> storeOutput value
        _ -> do
            let update = M.singleton pos value
            board <- gets s3dsNextBoard
            let newCells = M.union update (cells board)
            let newBoard = board { cells = newCells }
            -- FIXME: according to the spec:
            --      6. In every tick, all reads (and removals) happen before all the writes.
            -- However, we do removals with `writeTo _ Empty`. This should be fixed to align with the spec.
            --
            -- TODO: implement submitting of answers (by overwriting the OutputS operator)
            modify' $ \s -> s { s3dsNextBoard = newBoard }

storeOutput :: Cell -> Sim3dM ()
storeOutput result = do
    currentOutput <- gets s3dsOutput
    case currentOutput of
        Empty -> modify $ \s -> s { s3dsOutput = result }
        value ->
            when (value /= result) $
                sim3dThrowError $
                    "Error: trying to submit \"" <> DT.pack (show result) <>
                    "\" when \"" <> DT.pack (show currentOutput) <> "\" is already submitted"

moveCell :: Position -> Position -> Sim3dM ()
moveCell from to = do
    cell <- readAt from
    writeTo to cell
    writeTo from Empty

moveNextToCurrent :: Sim3dM ()
moveNextToCurrent = do
    curBoard <- gets s3dsCurBoard
    modify $ \s -> s { s3dsPreviousBoards = curBoard : (s3dsPreviousBoards s) }

    nextBoard <- gets s3dsNextBoard
    let normalized = normalize nextBoard
    modify $ \s -> s { s3dsCurBoard = normalized, s3dsNextBoard = normalized }

updateCells :: Sim3dM ()
updateCells = do
    board <- gets s3dsCurBoard
    mapM_ updateCell (M.keys $ cells board)

updateCell :: Position -> Sim3dM ()
updateCell pos@(x, y) = do
    cell <- readAt pos
    case cell of
        MoveLeft -> moveCell (x + 1, y) (x - 1, y)
        MoveRight -> moveCell (x - 1, y) (x + 1, y)
        MoveUp -> moveCell (x, y + 1) (x, y - 1)
        MoveDown -> moveCell (x, y - 1) (x, y + 1)
        Plus -> performArithmetic (+) pos
        Minus -> performArithmetic (-) pos
        Multiply -> performArithmetic (*) pos
        Divide -> performArithmetic quot pos
        Modulo -> performArithmetic rem pos
        TimeWarp -> warpTime pos
        Equal -> performComparison (==) pos
        NotEqual -> performComparison (/=) pos
        Value _ -> pure ()
        Empty -> pure ()
        OutputS -> pure ()
        InputA -> inputVarsDuringSimulation
        InputB -> inputVarsDuringSimulation
        where
            inputVarsDuringSimulation =
                sim3dThrowError
                    $ "Error at position " <> DT.pack (show pos)
                    <> ": input variables on the board during the simulation!"

performArithmetic :: (Integer -> Integer -> Integer) -> Position -> Sim3dM ()
performArithmetic op (x, y) = do
    v1 <- readAt (x - 1, y    )
    v2 <- readAt (x    , y - 1)
    case (v1, v2) of
        (Value a, Value b) -> do
            let result = a `op` b
            writeTo (x - 1, y) Empty
            writeTo (x, y - 1) Empty
            writeTo (x + 1, y) (Value result)
            writeTo (x, y + 1) (Value result)
        _ -> pure ()

performComparison :: (Cell -> Cell -> Bool) -> Position -> Sim3dM ()
performComparison cmp (x, y) = do
    v1 <- readAt (x - 1, y    )
    v2 <- readAt (x    , y - 1)
    when (v1 `cmp` v2) $ do
        moveCell (x-1, y) (x+1, y)
        moveCell (x, y-1) (x, y+1)

warpTime :: Position -> Sim3dM ()
warpTime (x, y) = do
    v <- readAt (x, y-1)
    dx' <- readAt (x-1, y)
    dy' <- readAt (x+1, y)
    dt' <- readAt (x, y+1)
    when (v /= Empty) $ do
      case (dx', dy', dt') of
          (Value dx, Value dy, Value dt) -> do
              when (dt < 1) $
                  sim3dThrowError $ "Error: attempted to warp time with dt="
                      <> DT.pack (show dt) <> " < 1"
              curTime <- gets s3dsTime
              when (dt >= curTime) $
                  sim3dThrowError $ "Error: attempted to warp time with dt="
                      <> DT.pack (show dt) <> " >= current time"
              prevBoards <- gets s3dsPreviousBoards
              let board = head $ drop (fromIntegral $ dt - 1) prevBoards
              let update = M.singleton (x-dx, y-dy) v
              let updatedBoard = normalize $ board { cells = M.union update (cells board) }
              -- TODO: check for interactions between multiple warp operators
              modify $ \s -> s { s3dsNextBoard = updatedBoard }
              modify $ \s -> s { s3dsPreviousBoards = drop (fromIntegral dt) prevBoards }
          _ -> pure ()

sim3dThrowError :: DT.Text -> Sim3dM ()
sim3dThrowError msg = do
    curTime <- gets s3dsTime
    throwError $ "[T " <> DT.pack (show curTime) <> "] " <> msg

-- Clean up Empty cells.
normalize :: Board -> Board
normalize board =
    let filteredCells = M.filter (/= Empty) $ cells board
        minX = if null filteredCells then 0 else minimum $ map fst $ M.keys filteredCells
        minY = if null filteredCells then 0 else minimum $ map snd $ M.keys filteredCells
        maxX = if null filteredCells then 0 else maximum $ map fst $ M.keys filteredCells
        maxY = if null filteredCells then 0 else maximum $ map snd $ M.keys filteredCells
        newMin = (minX, minY)
        newMax = (maxX, maxY) in
    board { cells = filteredCells, minCoords = newMin, maxCoords = newMax }

shiftBy :: (Integer, Integer) -> Board -> Board
shiftBy (dx, dy) board =
    let shiftCell (x, y) = (x + dx, y + dy)
        shiftedCells = M.mapKeys (shiftCell) $ cells board
        (minX, minY) = minCoords board
        (maxX, maxY) = maxCoords board
        newMin = (minX + dx, minY + dy)
        newMax = (maxX + dx, maxY + dy) in
    board { cells = shiftedCells, minCoords = newMin, maxCoords = newMax }

showBoard :: Sim3dM DT.Text
showBoard = do
    let showCell Empty = "."
        showCell (Value x) = DT.pack $ show x
        showCell MoveLeft = "<"
        showCell MoveRight = ">"
        showCell MoveUp = "^"
        showCell MoveDown = "v"
        showCell Plus = "+"
        showCell Minus = "-"
        showCell Multiply = "*"
        showCell Divide = "/"
        showCell Modulo = "%"
        showCell TimeWarp = "@"
        showCell Equal = "="
        showCell NotEqual = "#"
        showCell OutputS = "S"
        showCell InputA = "A"
        showCell InputB = "B"

    columns <- boardToColumnMajorList
    let formattedColumns = (map.map) showCell columns
        columnWidths = map (\column -> maximum (0 : map DT.length column)) formattedColumns
        paddedColumns =
            zipWith
                (\width column -> map (DT.center width ' ') column)
                columnWidths
                formattedColumns
        rows = transpose paddedColumns
        table = DT.unlines $ map DT.unwords rows
    pure table

boardToColumnMajorList :: Sim3dM [[Cell]]
boardToColumnMajorList = do
    board <- gets s3dsCurBoard
    let ((minX, minY), (maxX, maxY)) = (minCoords board, maxCoords board)
    pure $
        map
            (\x ->
                map
                    (\y -> M.findWithDefault Empty (x, y) $ cells board)
                    [minY..maxY])
            [minX..maxX]
