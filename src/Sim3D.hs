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
  
type Board = V.Vector (V.Vector Cell)

isNumber :: DT.Text -> Bool
isNumber text =
    case DT.unpack text of
        [] -> False
        ('-':digits) -> all C.isDigit digits
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
simulateStep board = undefined
