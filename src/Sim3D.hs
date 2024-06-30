module Sim3D (simulate, simulateStep, parseBoard, Board, stateFromBoard, Sim3dState(..), shiftBy) where

import qualified Data.Char as C
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import qualified Data.Map as M
import Control.Monad.State

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

data Board = Board {
    cells :: M.Map (Integer, Integer) Cell,
    minCoords :: (Integer, Integer),
    maxCoords :: (Integer, Integer)
} deriving (Eq, Show)

getCell :: Board -> (Integer, Integer) -> Cell
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

boardFromList :: [[Cell]] -> Board
boardFromList cells =
    let indexedCells = concat $ zipWith (\y row -> zipWith (\x cell -> ((x, y), cell)) [0..] row) [0..] cells
        minX = minimum $ map (fst . fst) indexedCells
        minY = minimum $ map (snd . fst) indexedCells
        maxX = maximum $ map (fst . fst) indexedCells
        maxY = maximum $ map (snd . fst) indexedCells in
    Board (M.fromList indexedCells) (minX, minY) (maxX, maxY)

boardToList :: Board -> [[Cell]]
boardToList board =
    let ((minX, minY), (maxX, maxY)) = (minCoords board, maxCoords board) in
    map (\y -> map (\x -> M.findWithDefault Empty (x, y) $ cells board) [minX..maxX]) [minY..maxY]

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
printBoard board = do
    let printCell Empty = putStr "."
        printCell (Value x) = putStr $ show x
        printCell MoveLeft = putStr "<"
        printCell MoveRight = putStr ">"
        printCell MoveUp = putStr "^"
        printCell MoveDown = putStr "v"
        printCell Plus = putStr "+"
        printCell Minus = putStr "-"
        printCell Multiply = putStr "*"
        printCell Divide = putStr "/"
        printCell Modulo = putStr "%"
        printCell TimeWarp = putStr "@"
        printCell Equal = putStr "="
        printCell NotEqual = putStr "#"
        printCell OutputS = putStr "S"
        printCell InputA = putStr "A"
        printCell InputB = putStr "B"
        printRow row = do
            mapM_ (\x -> do
                printCell x
                putStr " ") row

            putStrLn ""
    mapM_ printRow $ boardToList board

simulate :: String -> Inputs -> IO ()
simulate boardPath inputs = do
    board <- readBoard boardPath
    let board' = replaceInputs board inputs
        state = stateFromBoard board'
        isAlreadyOver = evalState isGameOver state
    if isAlreadyOver then do
        putStrLn "I cannot see the goal on the initial board. This game will never end."
        doSimulation board' False
    else
        doSimulation board' True

doSimulation :: Board -> Bool -> IO ()
doSimulation board checkGameOver = do
    printBoard board
    let state = stateFromBoard board
    let gameOver = evalState isGameOver state
    if checkGameOver && gameOver then
        putStrLn "Game over!"
    else do
        putStrLn "Press enter to continue."
        _ <- getLine
        let newBoard = s3dsCurBoard $ execState simulateStep state
        doSimulation newBoard checkGameOver

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

type Sim3dM a = State Sim3dState a

isGameOver :: Sim3dM Bool
isGameOver = pure True

simulateStep :: Sim3dM ()
simulateStep = do
    board <- gets s3dsCurBoard
    updateCells
    moveNextToCurrent

type Update = (Integer, Integer, Cell)

readAt :: (Integer, Integer) -> Sim3dM Cell
readAt pos = do
    board <- gets s3dsCurBoard
    pure $ getCell board pos

writeTo :: (Integer, Integer) -> Cell -> Sim3dM ()
writeTo pos value = do
    let update = M.singleton pos value
    board <- gets s3dsNextBoard
    let newCells = M.union update (cells board)
    let newBoard = board { cells = newCells }
    modify' $ \s -> s { s3dsNextBoard = newBoard }

moveCell :: (Integer, Integer) -> (Integer, Integer) -> Sim3dM ()
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

updateCell :: (Integer, Integer) -> Sim3dM ()
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
        Value _ -> pure ()
        Empty -> pure ()
        _ -> pure () -- TODO: implement actions for the other cells

performArithmetic :: (Integer -> Integer -> Integer) -> (Integer, Integer) -> Sim3dM ()
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
