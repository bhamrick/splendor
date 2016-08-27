{-# LANGUAGE OverloadedStrings #-}
module Server.GameLogs (writeGame) where

import Data.Aeson
import qualified Data.ByteString.Lazy as ByteString
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Time.Calendar
import Data.Time.Clock
import System.Directory
import System.FilePath
import System.IO

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
    ByteString.writeFile (directory </> (gameKey <> ".json")) (encode gameJson)
