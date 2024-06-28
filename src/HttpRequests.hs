module HttpRequests (performRequest) where

import Config
import qualified Data.Text.IO as I
import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

performRequest :: String -> IO ()
performRequest requestText = do
    token <- readApiToken
    putStrLn $ "Performing request: " ++ requestText
    let body = BL.pack $ requestText
    req <- parseRequest "POST https://boundvariable.space/communicate"
    let bearerBS = encodeUtf8 $ T.pack $ "Bearer " ++ (T.unpack token)
    response <- httpLBS
     $ setRequestBodyLBS body
     $ setRequestHeader "Authorization" [bearerBS] req
    BL.putStrLn $ getResponseBody response
