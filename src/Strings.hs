module Strings where

import Data.Char
import qualified Data.Text as T

stringRemap = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

remapChar :: Char -> Char
remapChar c
    | ord c < 33 || ord c > 126 = c
    | otherwise = stringRemap !! (ord c - 33)

remapString :: T.Text -> T.Text
remapString = T.map remapChar
