module Lib
    ( parseNumber
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

parseNumber :: T.Text -> Maybe Integer
parseNumber input =
  case T.uncons input of
    Just ('I', base94) ->
      if T.null base94
        then Nothing
        else
          T.foldl'
            (\acc digit -> ((,) <$> (digit `M.lookup` alphabet) <*> acc) >>= \(i, a) -> pure (a*94 + i))
            (Just 0)
            base94
    _ -> Nothing
  where
    alphabet = M.fromList $ zip ['!' .. '~'] [0..]
