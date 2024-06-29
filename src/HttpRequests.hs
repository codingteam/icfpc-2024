module HttpRequests (performRequest, performDownloadAllKnown) where

import Config
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

import Text.Pretty.Simple (pPrint)

import Lib
import Strings
import Parser
import AST
import Printer

performRequest :: T.Text -> IO ()
performRequest bodyToSend = do
    token <- readApiToken
    req <- parseRequest "POST https://boundvariable.space/communicate"
    let bearerHeader = T.concat ["Bearer ", token]

    BS.putStrLn $ BS.concat ["Sending request in Galaxy: ", DTE.encodeUtf8 bodyToSend]
    response <- httpLBS
     $ setRequestBodyLBS (BS.fromStrict $ DTE.encodeUtf8 bodyToSend)
     $ setRequestHeader "Authorization" [DTE.encodeUtf8 bearerHeader] req
    let responseBody = getResponseBody response

    BL.putStrLn $ BL.concat ["Received response in Galaxy: ", responseBody]

    case parseExpression (DTE.decodeUtf8 $ BS.concat . BL.toChunks $ responseBody) of
        Left err -> putStrLn $ "Cannot parse: " ++ err
        Right program ->
            case evalAst program of
                Left err -> DTI.putStrLn $ "Cannot evaluate: " <> err
                Right result -> pPrint result

allKnownData :: [String]
allKnownData = []
    ++ ["index", "echo", "scoreboard", "lambdaman", "spaceship", "language_test", "3d", "efficiency", "3d-example"]
    ++ ["lambdaman" ++ show n | n <- [1..21] :: [Int]]
    ++ ["spaceship" ++ show n | n <- [1..25] :: [Int]]
    ++ ["3d" ++ show n | n <- [1..12] :: [Int]]
    ++ ["efficiency" ++ show n | n <- [1..13] :: [Int]]

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
