module HttpRequests (performRequest) where

import Config
import qualified Data.Text.IO as I
import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Strings as S

performRequest :: String -> IO ()
performRequest requestText = do
    token <- readApiToken
    putStrLn $ "Performing request: " ++ requestText
    let galaxyBody = S.textToGalaxy (T.pack requestText)
    let body = encodeUtf8 $ galaxyBody
    req <- parseRequest "POST https://boundvariable.space/communicate"
    let bearerBS = encodeUtf8 $ T.pack $ "Bearer " ++ (T.unpack token)

    BS.putStrLn $ BS.concat ["Sending request in Galaxy: ", encodeUtf8 galaxyBody]
    response <- httpLBS
     $ setRequestBodyLBS (BS.fromStrict body)
     $ setRequestHeader "Authorization" [bearerBS] req
    let responseBody = getResponseBody response

    if BL.index responseBody 0 == 'S' then
        let galaxyEncodedBody = BL.drop 1 responseBody in
        putStrLn $ "Received response in Galaxy:\n" ++ (T.unpack $ S.textFromGalaxy $ L.toStrict $ decodeUtf8 $ galaxyEncodedBody)
    else
        BL.putStrLn $ BL.concat ["Received raw response: ", responseBody]
