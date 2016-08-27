{-# LANGUAGE OverloadedStrings #-}
module Server.GameLogs (writeGame, archiveR) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS 
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text as Text
import Data.Time.Calendar
import Data.Time.Clock
import Network.HTTP.Types
import Network.Wai
import System.Directory
import System.FilePath
import System.IO

import Lucid hiding (for_)

import Server.Types
import Splendor.Types

mainDirectory :: FilePath
mainDirectory = "games"

dayDirectory :: Day -> FilePath
dayDirectory d = mainDirectory </> showGregorian d

writeGame :: String -> RunningGame GameState -> GameResult -> UTCTime -> IO ()
writeGame gameKey completeGame res ts = do
    let gameJson = object
            [ "result" .= res
            , "gameData" .= completeGame
            ]
        directory = dayDirectory (utctDay ts)
    createDirectoryIfMissing True directory
    LBS.writeFile (directory </> (gameKey <> ".json")) (encode gameJson)

archiveR :: Request -> IO Response
archiveR req = do
    case pathInfo req of
        [base] -> do
            days <- listDirectory mainDirectory
            let htmlResp = do
                    doctype_
                    head_ $ do
                        title_ "Splendor"
                    body_ $ do
                        ul_ $ do
                            for_ days $ \d ->
                                li_ $
                                    a_ [href_ ("/" <> base <> "/" <> Text.pack d)] $ toHtml d
            pure $ responseLBS status200 [("Content-Type", "text/html")] (renderBS htmlResp)
        [base, day] -> do
            games <- listDirectory (mainDirectory </> Text.unpack day)
            let htmlResp = do
                    doctype_
                    head_ $ do
                        title_ "Splendor"
                    body_ $ do
                        ul_ $ do
                            for_ games $ \g ->
                                li_ $
                                    a_ [href_ ("/" <> base <> "/" <> day <> "/" <> Text.pack g)] $ toHtml g
            pure $ responseLBS status200 [("Content-Type", "text/html")] (renderBS htmlResp)
        [base, day, game] -> do
            dat <- LBS.readFile (mainDirectory </> Text.unpack day </> Text.unpack game)
            pure $ responseLBS status200 [("Content-Type", "application/json")] dat
        _ -> do
            pure $ responseLBS status404 [("Content-Type", "text/plain")] "Not found"
