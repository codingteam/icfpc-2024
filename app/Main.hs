module Main (main) where

import System.Environment (getArgs)
import qualified Data.Text as T

import Lib

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

    _ -> putStrLn "Unknown arguments. See app/Main.hs for a list of available commands"
