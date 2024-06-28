module Lib
    ( parseNumber
    , parseNumberToken
    , numberToGalaxy
    ) where

import Data.Maybe (fromJust)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Text as T

--- Parses number token, i.e. a string starting with "I".
parseNumberToken :: T.Text -> Maybe Integer
parseNumberToken input =
  case T.uncons input of
    Just ('I', base94) -> parseNumber base94
    _ -> Nothing

--- Parses a base94 number.
parseNumber :: T.Text -> Maybe Integer
parseNumber input
  | T.null input = Nothing
  | otherwise =
      T.foldl'
        (\acc digit -> ((,) <$> (digit `M.lookup` alphabet) <*> acc) >>= \(i, a) -> pure (a*94 + i))
        (Just 0)
        input
  where
    alphabet = M.fromList $ zip ['!' .. '~'] [0..]

numberToGalaxy :: Integer -> T.Text
numberToGalaxy 0 = "!"
numberToGalaxy number = T.pack $ go [] number
  where
    go acc 0 = acc
    go acc i = go (digitToChar (fromIntegral $ i `mod` 94) : acc) (i `div` 94)

    digitToChar i = fromJust $ i `IM.lookup` alphabet
    alphabet = IM.fromList $ zip [0..] ['!' .. '~']
