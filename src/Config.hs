module Config (readApiToken) where

import qualified Data.Text.IO as I
import Data.Text

readApiToken :: IO Text
readApiToken = do
    token <- I.readFile "TOKEN.txt"
    putStrLn "Token successfully read from TOKEN.txt."
    return $ strip token
