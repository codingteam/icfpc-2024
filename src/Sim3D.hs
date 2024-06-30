module Sim3D (
    simulate
,   simulateStep
,   parseBoard
,   Board
,   stateFromBoard
,   Sim3dState(..)
,   Sim3dError
,   shiftBy
,   execSimulation
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
        doSimulation board' False
    else
        doSimulation board' True

doSimulation :: Board -> Bool -> IO ()
doSimulation board checkGameOver = do
    printBoard board
    let s3dState = stateFromBoard board
        gameOver =
            let result = evalSimulation isGameOver s3dState
            in isLeft result || (result == Right True)
    if checkGameOver && gameOver then
        putStrLn "Game over!"
    else do
        putStrLn "Press enter to continue."
        _ <- getLine
        case execSimulation simulateStep s3dState of
            Left err -> DTI.putStrLn $ "The game ended: " <> err
            Right s -> doSimulation (s3dsCurBoard s) checkGameOver

data Sim3dState = Sim3dState {
    s3dsCurBoard :: Board --- ^ Current state of the board (read-only)
,   s3dsNextBoard :: Board --- ^ Next state of the board (we're updating it)
}

stateFromBoard :: Board -> Sim3dState
stateFromBoard board =
    Sim3dState {
        s3dsCurBoard = board
    ,   s3dsNextBoard = board
    }

type Sim3dError = DT.Text
type Sim3dM a = StateT Sim3dState (Except Sim3dError) a

execSimulation :: Sim3dM a -> Sim3dState -> Either Sim3dError Sim3dState
execSimulation action initialState =
    runExcept $ execStateT action initialState

evalSimulation :: Sim3dM a -> Sim3dState -> Either Sim3dError a
evalSimulation action initialState =
    runExcept $ evalStateT action initialState

isGameOver :: Sim3dM Bool
isGameOver = pure True

simulateStep :: Sim3dM ()
simulateStep = do
    updateCells
    moveNextToCurrent

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

    -- we haven't written to this cell on this tick yet
    let isFirstWrite = currentValue == nextValue
    -- we're writing the same value that's already in the cell
    let overwriteWithSame = nextValue == value

    when (not isFirstWrite && not overwriteWithSame) $ do
        let fPos = DT.pack $ show pos
        let fValue = DT.pack $ show value
        let fNextValue = DT.pack $ show nextValue
        throwError $
            "Error: trying to overwrite previously written value of \""
            <> fNextValue <> "\" with \"" <> fValue <> "\" at " <> fPos

writeTo :: Position -> Cell -> Sim3dM ()
writeTo pos value = do
    ensureWriteIsAllowed pos value
    let update = M.singleton pos value
    board <- gets s3dsNextBoard
    let newCells = M.union update (cells board)
    let newBoard = board { cells = newCells }
    modify' $ \s -> s { s3dsNextBoard = newBoard }

moveCell :: Position -> Position -> Sim3dM ()
moveCell from to = do
    cell <- readAt from
    writeTo to cell
    writeTo from Empty

moveNextToCurrent :: Sim3dM ()
moveNextToCurrent = do
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
        Divide -> performArithmetic div pos
        Modulo -> performArithmetic mod pos
        Equal -> performComparison (==) pos
        NotEqual -> performComparison (/=) pos
        Value _ -> pure ()
        Empty -> pure ()
        op -> throwError $ "Error: operator " <> (DT.pack $ show op) <> " is not implemented yet"

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
