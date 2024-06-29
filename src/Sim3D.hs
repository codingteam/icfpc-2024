module Sim3D (simulate, simulateStep, parseBoard) where

import qualified Data.Char as C
import qualified Data.Vector as V
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import qualified Data.Map as M

data Cell =
      Empty
    | Value Int
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

type Inputs = (Int, Int)

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
        isAlreadyOver = isGameOver board'
    if isAlreadyOver then do
        putStrLn "I cannot see the goal on the initial board. This game will never end."
        doSimulation board' False
    else
        doSimulation board' True

doSimulation :: Board -> Bool -> IO ()
doSimulation board checkGameOver = do
    printBoard board
    if checkGameOver && isGameOver board then
        putStrLn "Game over!"
    else do
        putStrLn "Press enter to continue."
        _ <- getLine
        doSimulation (simulateStep board) checkGameOver

isGameOver :: Board -> Bool
isGameOver board = True

simulateStep :: Board -> Board
simulateStep board =
    let effects = produceUpdates board in
    normalize $ applyUpdates board effects

type Update = (Integer, Integer, Cell)

produceCellUpdate ::  Board -> (Integer, Integer) -> [Update]
produceCellUpdate board (x, y) =
    let cell = getCell board (x, y) in
    case cell of
        MoveLeft -> moveCell (x + 1, y) (x - 1, y) board
        MoveRight -> moveCell (x - 1, y) (x + 1, y) board
        MoveUp -> moveCell (x, y + 1) (x, y - 1) board
        MoveDown -> moveCell (x, y - 1) (x, y + 1) board
        Value _ -> []
        Empty -> []
        _ -> [] -- TODO: implement actions for the other cells

    where moveCell (x1, y1) (x2, y2) board = [(x1, y1, Empty), (x2, y2, getCell board (x1, y1))]

produceUpdates :: Board -> [Update]
produceUpdates board = concatMap (produceCellUpdate board) $ M.keys $ cells board

applyUpdates :: Board -> [Update] -> Board
applyUpdates board updates =
    let updateMap = M.fromList $ map (\(x, y, cell) -> ((x, y), cell)) updates
        newCells = M.union updateMap $ cells board in
    board { cells = newCells }      

-- Brings the whole board back into coordinate (0, 0), clean up Empty cells
normalize :: Board -> Board
normalize board =
    let filteredCells = M.filter (/= Empty) $ cells board
        minX = if null filteredCells then 0 else minimum $ map fst $ M.keys filteredCells
        minY = if null filteredCells then 0 else minimum $ map snd $ M.keys filteredCells 
        maxX = if null filteredCells then 0 else maximum $ map fst $ M.keys filteredCells
        maxY = if null filteredCells then 0 else maximum $ map snd $ M.keys filteredCells
        shiftX = -minX
        shiftY = -minY
        newCells = M.mapKeys (\(x, y) -> (x + shiftX, y + shiftY)) filteredCells
        newMin = (0, 0)
        newMax = (maxX + shiftX, maxY + shiftY) in
    board { cells = newCells, minCoords = newMin, maxCoords = newMax }
    