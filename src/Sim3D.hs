module Sim3D (simulate, simulateStep, parseBoard) where

import qualified Data.Char as C
import qualified Data.Vector as V
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

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

-- Row-major order: outer vector is indexed by rows, inner vector is indexed by
-- column
type Board = V.Vector (V.Vector Cell)

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

parseBoard :: DT.Text -> Board
parseBoard contents =
    let textRows = DT.lines contents
        textCells = map DT.words textRows in
    V.fromList $ map (V.fromList . map readCell) textCells

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
        replaceRow row = V.map replaceCell row
    in V.map replaceRow board

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
            V.mapM_ (\x -> do
                printCell x
                putStr " ") row

            putStrLn ""
    V.mapM_ printRow board

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
    applyUpdates board effects

type Update = (Int, Int, Cell)

produceCellUpdate :: (Int, Int) -> Board -> [(Int, Int, Cell)]
produceCellUpdate (x, y) board =
    let cell = (board V.! y) V.! x in
    case cell of
        MoveLeft -> moveCell (x + 1, y) (x - 1, y) board
        MoveRight -> moveCell (x - 1, y) (x + 1, y) board
        MoveUp -> moveCell (x, y + 1) (x, y - 1) board
        MoveDown -> moveCell (x, y - 1) (x, y + 1) board
        Value _ -> []
        Empty -> []
        _ -> error $ "Unexpected cell: " ++ show cell ++ " at " ++ show (x, y) ++ " in " ++ show board ++ "."

    where moveCell (x1, y1) (x2, y2) board = [(x1, y1, Empty), (x2, y2, (board V.! y1) V.! x1)]

produceUpdates :: Board -> [Update]
produceUpdates board =
    V.toList $
        V.concatMap id $ V.imap (\r row ->
            V.concatMap id $ V.imap (\c _ -> (V.fromList $ produceCellUpdate (c, r) board)) row) board

applyUpdates :: Board -> [Update] -> Board
applyUpdates board [] = board
applyUpdates board ((x, y, cell):updates) =
    let row = board V.! y
        newRow = row V.// [(x, cell)]
        newBoard = board V.// [(y, newRow)] in
    applyUpdates newBoard updates
