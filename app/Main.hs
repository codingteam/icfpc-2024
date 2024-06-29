module Main (main) where

import HttpRequests (performRequest, performRequest', performDownloadAllKnown)
import System.Environment

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Pretty.Simple (pPrint)

import Lib
import Strings
import Parser
import AST
import Printer
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

    ["lambdaman", path] -> do
        problem <- problemFromFile path
        Just sol <- evalAStar problem
        print sol

    ["http", request] -> performRequest request
    ["http-raw", request] -> performRequest' False request
    ["http-eval-galaxy", path] -> do
        txt <- TIO.readFile path
        case parseExpression txt of
          Right program -> do
            let echoProgram = Concat (Str "echo ") program
                echoGalaxy = astToGalaxy echoProgram
            performRequest' False (T.unpack echoGalaxy)
          Left err -> putStrLn $ "Failed to parse the file: " <> err

    ["http-all"] -> performDownloadAllKnown
    _ -> printHelp
