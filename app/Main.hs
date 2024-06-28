module Main (main) where

import HttpRequests (performRequest)
import System.Environment

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Lib
import Strings
import Parser

printHelp :: IO ()
printHelp = putStrLn "Possible args:\n - http <request>"

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
        case parseExpression txt of
            Left e -> putStrLn e
            Right ast -> putStrLn $ show ast

    ["http", request] -> performRequest request
    _ -> printHelp
