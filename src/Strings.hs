module Strings where

import Data.Char
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Monoid ((<>))

fromGalaxy :: [Char]
fromGalaxy = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

toGalaxy :: [Char]
toGalaxy = [ chr $ 33 + toGalaxyMap M.! c | c <- fromGalaxy ]

toGalaxyMap :: M.Map Char Int
toGalaxyMap = M.fromList $ zip fromGalaxy [0..]

charFromGalaxy :: Char -> Char
charFromGalaxy c
    | ord c < 33 || ord c > 126 = c
    | otherwise = fromGalaxy !! (ord c - 33)

textToGalaxy :: T.Text -> T.Text
textToGalaxy = ("S" <>) . T.map charToGalaxy

charToGalaxy :: Char -> Char
charToGalaxy c
    | c `elem` fromGalaxy = chr $ 33 + toGalaxyMap M.! c
    | otherwise = c

textFromGalaxy :: T.Text -> T.Text
textFromGalaxy = T.map charFromGalaxy
