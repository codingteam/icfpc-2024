module Main (main) where

import HttpRequests (performRequest, performDownloadAllKnown)
import System.Environment
import Data.Foldable (minimumBy)
import Data.Ord (comparing)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Pretty.Simple (pPrint)

import Lib
import Strings
import Parser
import AST
import Printer
import StringBitCoding
import Lambdaman

printHelp :: IO ()
printHelp = putStrLn "Possible args:\n - http <request>\n- http-all"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["parseNumber"] -> do
      putStrLn "Please enter a number (with out without leading \"I\"):"
      number <- getLine
      case number of
        [] -> putStrLn "This is an empty string, not a number!"
        ['I'] -> putStrLn "Number token must have a non-empty body!"
        ('I':digits) -> print $ parseNumber (T.pack digits)
        digits -> print $ parseNumber (T.pack digits)

    ["fileToGalaxy", path] -> do
        txt <- TIO.readFile path
        TIO.putStr $ textToGalaxy txt

    ["fileFromGalaxy", path] -> do
        txt <- TIO.readFile path
        TIO.putStr $ textFromGalaxy txt

    ["parseFile", path] -> do
        txt <- TIO.readFile path
        case parseExpression (T.strip txt) of
            Left e -> putStrLn e
            Right ast -> pPrint ast

    ["evalFile", path] -> do
        txt <- TIO.readFile path
        case parseExpression (T.strip txt) of
            Left err -> putStrLn $ "Cannot parse: " ++ err
            Right program ->
                case evalAst program of
                    Left err -> TIO.putStrLn $ "Cannot evaluate: " <> err
                    Right result -> pPrint result

    ["http", request] -> performRequest $ textToGalaxy $ T.pack request
    ["http-raw", request] -> performRequest $ T.pack request

    ["lambdaman", path] -> do
        problem <- problemFromFile path
        Just sol <- evalAStar problem
        print sol

    ["greedy-lambdaman", path] -> do
        problem <- problemFromFile path
        let sol = greedySolve problem
        putStrLn $ showPath sol

    ["http-eval-galaxy", path] -> do
        txt <- TIO.readFile path
        case parseExpression txt of
          Right program -> do
            let echoProgram = Concat (Str "echo ") program
                echoGalaxy = astToGalaxy echoProgram
            performRequest echoGalaxy
          Left err -> putStrLn $ "Failed to parse the file: " <> err

    ["http-lambdaman-solution-using-bitcoding", problem_no, solution] -> do
        let packed = T.pack solution
            compressed = toSelfExtractingBitcode lambdamanAlphabet packed
            instruction = "solve lambdaman" <> (T.pack problem_no) <> " "
            program = Concat (Str instruction) compressed
            verbatimProgram = instruction <> packed
            galaxy = minimumBy (comparing T.length) [astToGalaxy program, textToGalaxy verbatimProgram]
        performRequest galaxy

    ["http-all"] -> performDownloadAllKnown

    ["upload", problem, path] -> do
        txt <- TIO.readFile path
        performRequest $ textToGalaxy $ "solve " <> (T.pack problem) <> " " <> txt

    ["test-3d", a, b, path] -> do
        txt <- TIO.readFile path
        performRequest $ textToGalaxy $ "test 3d " <> (T.pack a) <> " " <> (T.pack b) <> "\n" <> txt

    _ -> printHelp
