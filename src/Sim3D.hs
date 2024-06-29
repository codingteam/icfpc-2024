module Sim3D (simulate) where

import qualified Data.Char as C
import qualified Data.Vector as V
  
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
  
type Board = V.Vector (V.Vector Cell)

isNumber :: String -> Bool
isNumber ('-' : digits) = all C.isDigit digits
isNumber digits = all C.isDigit digits

readCell :: String -> Cell
readCell "." = Empty
readCell x | isNumber x = Value $ read x
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

readBoard :: String -> IO Board
readBoard path = do
    contents <- readFile path
    let textRows = lines contents
        textCells = map words textRows
    return $ V.fromList $ map (V.fromList . map readCell) textCells    

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
    doSimulation board'    
        
doSimulation :: Board -> IO ()
doSimulation board = do
    printBoard board
    if isGameOver board then
        putStrLn "Game over!"
    else do
        putStrLn "Press enter to continue."
        _ <- getLine
        doSimulation $ simulateStep board    

isGameOver :: Board -> Bool
isGameOver board = True

simulateStep :: Board -> Board
simulateStep board = undefined
