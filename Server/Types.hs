{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Server.Types where

import Control.Lens
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics
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
    deriving (Eq, Show, Ord)

data Instance a =
    Instance
        { _playerKeys :: Map String Int
        , _runningGame :: RunningGame a
        }
    deriving (Eq, Show, Ord)

data Lobby a =
    Lobby
        { _waitingPlayers :: [(String, PlayerInfo)]
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
    = GameAction Action
    | NewLobby
    | JoinLobby String PlayerInfo
    | ListLobbies
    | GetGameState String
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
