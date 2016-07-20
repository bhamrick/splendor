{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Server.Types where

import Control.Lens
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Map (Map, mapKeys)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Traversable
import GHC.Generics
import Text.Read
import Splendor.Types

data PlayerInfo =
    PlayerInfo
        { _displayName :: Text
        }
    deriving (Eq, Show, Ord, Generic)

instance ToJSON PlayerInfo
instance FromJSON PlayerInfo

data RunningGame a =
    RunningGame
        { _players :: Map Int PlayerInfo
        , _version :: Int
        , _gameState :: a
        }
    deriving (Eq, Show, Ord, Functor, Generic)

instance ToJSON (Map Int PlayerInfo) where
    toJSON = toJSON . mapKeys show
    toEncoding = toEncoding . mapKeys show

instance FromJSON (Map Int PlayerInfo) where
    parseJSON val = do
        m <- parseJSON val
        assocList <- for (Map.toList m) $ \(k, v) -> do
            case readMaybe k of
                Nothing -> fail "Invalid color"
                Just c -> pure (c, v)
        pure (Map.fromList assocList)

instance ToJSON a => ToJSON (RunningGame a)
instance FromJSON a => FromJSON (RunningGame a)

data Instance a =
    Instance
        { _playerKeys :: Map String Int
        , _runningGame :: RunningGame a
        }
    deriving (Eq, Show, Ord)

data Lobby a =
    Lobby
        { _waitingPlayers :: [(String, PlayerInfo)]
        , _ownerKey :: String
        }
    deriving (Eq, Show, Ord)

data ServerRequest a =
    ServerRequest
        { _playerKey :: String
        , _requestData :: a
        }
    deriving (Eq, Show, Ord, Generic)

instance ToJSON a => ToJSON (ServerRequest a)
instance FromJSON a => FromJSON (ServerRequest a)

data RequestData
    = ListLobbies
    | NewLobby PlayerInfo
    | JoinLobby String PlayerInfo
    | StartGame String
    | GetGameState String
    | GameAction String Action
    deriving (Eq, Show, Ord, Generic)

instance ToJSON RequestData
instance FromJSON RequestData

data ServerState a =
    ServerState
        { _instances :: Map String (Instance a)
        , _lobbies :: Map String (Lobby a)
        }
    deriving (Eq, Show, Ord)

makeLenses ''PlayerInfo
makeLenses ''RunningGame
makeLenses ''Instance
makeLenses ''Lobby
makeLenses ''ServerState
makeLenses ''ServerRequest
