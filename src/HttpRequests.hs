module HttpRequests (performRequest, performDownloadAllKnown) where

import Config
import qualified Data.Text.IO as I
import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Lazy.Encoding as DTLE
import qualified Data.Text as T
import qualified Data.Text.IO as DTI
import qualified Data.Text.Lazy as L
import qualified Strings as S
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)

performRequest :: String -> IO ()
performRequest requestText = do
    token <- readApiToken
    putStrLn $ "Performing request: " ++ requestText
    let galaxyBody = S.textToGalaxy (T.pack requestText)
    req <- parseRequest "POST https://boundvariable.space/communicate"
    let bearerHeader = T.concat ["Bearer ", token]

    BS.putStrLn $ BS.concat ["Sending request in Galaxy: ", DTE.encodeUtf8 galaxyBody]
    response <- httpLBS
     $ setRequestBodyLBS (BS.fromStrict $ DTE.encodeUtf8 galaxyBody)
     $ setRequestHeader "Authorization" [DTE.encodeUtf8 bearerHeader] req
    let responseBody = getResponseBody response

    if BL.index responseBody 0 == 'S' then
        let galaxyEncodedBody = BL.drop 1 responseBody in
        putStrLn $ "Received response in Galaxy:\n" ++ (T.unpack $ S.textFromGalaxy $ L.toStrict $ DTLE.decodeUtf8 $ galaxyEncodedBody)
    else
        BL.putStrLn $ BL.concat ["Received raw response: ", responseBody]

allKnownData :: [String]
allKnownData =
    ["index", "echo", "scoreboard", "lambdaman", "spaceship"]
    ++ ["lambdaman" ++ show n | n <- [1..21]]

outDir :: String
outDir = "data"

performDownloadAllKnown :: IO ()
performDownloadAllKnown = do
    token <- readApiToken
    let bearerHeader = T.concat ["Bearer ", token]

    putStrLn "Downloading all known data."
    forM_ allKnownData $ \resource -> do
        let request = "get " ++ resource
        let body = S.textToGalaxy $ T.pack request

        putStrLn $ "Performing request: " ++ request

        req <- parseRequest "POST https://boundvariable.space/communicate"
        response <- httpLBS
             $ setRequestBodyLBS (BS.fromStrict $ DTE.encodeUtf8 body)
             $ setRequestHeader "Authorization" [DTE.encodeUtf8 bearerHeader] req
        let responseBody = getResponseBody response

        let rawFile = outDir ++ "/" ++ resource ++ ".glx"
        BL.writeFile rawFile responseBody
        if BL.index responseBody 0 == 'S' then
            let galaxyEncodedBody = BL.drop 1 responseBody
                textBody = S.textFromGalaxy $ L.toStrict $ DTLE.decodeUtf8 galaxyEncodedBody
                txtFile = outDir ++ "/" ++ resource ++ ".txt" in
            DTI.writeFile txtFile textBody
        else pure ()

        putStrLn "Sleeping for 4 sec due to rate limit."
        threadDelay 4000000
