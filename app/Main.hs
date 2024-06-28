module Main (main) where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Lib
import Strings

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["parseNumber"] -> do
      putStrLn "Please enter a number (starting with I):"
      number <- getLine
      case parseNumber (T.pack number) of
        Just x -> print x
        Nothing -> putStrLn "This is not a number. Perhaps you forgot the \"I\" indicator?"

    ["fileToGalaxy", path] -> do
        txt <- TIO.readFile path
        TIO.putStr $ textToGalaxy txt

    ["fileFromGalaxy", path] -> do
        txt <- TIO.readFile path
        TIO.putStr $ textFromGalaxy txt

    _ -> putStrLn "Unknown arguments. See app/Main.hs for a list of available commands"
