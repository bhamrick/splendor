{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Server.Types where

import Control.Lens
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Map (Map, mapKeys)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time.Clock
import Data.Traversable
import GHC.Generics
import Text.Read
import Splendor.Types

import Language.Record.Lens

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

data Instance
    = Instance
        { _name :: Text
        , _lastUpdated :: UTCTime
        , _details :: InstanceDetails
        }
    deriving (Eq, Show, Ord, Generic)

data InstanceDetails
    = WaitingInstance
        { _waitingPlayers :: [(String, PlayerInfo)]
        , _maxPlayers :: Int
        , _ownerKey :: String
        }
    | RunningInstance
        { _playerKeys :: Map String Int
        , _runningGame :: RunningGame GameState
        }
    | CompletedInstance
        { _playerKeys :: Map String Int
        , _completedGame :: RunningGame GameState
        , _result :: GameResult
        }
    deriving (Eq, Show, Ord, Generic)

data InstanceView
    = InstanceView
        { _name :: Text
        , _details :: InstanceViewDetails
        }
    deriving (Eq, Show, Ord, Generic)

instance ToJSON InstanceView
instance FromJSON InstanceView

data InstanceViewDetails
    = WaitingInstanceView
        { _waitingPlayers :: [PlayerInfo]
        , _maxPlayers :: Int
        }
    | RunningInstanceView
        { _runningGame :: RunningGame GameView
        }
    | CompletedInstanceView
        { _completedGame :: RunningGame GameState
        , _result :: GameResult
        }
    deriving (Eq, Show, Ord, Generic)

instance ToJSON InstanceViewDetails
instance FromJSON InstanceViewDetails

data InstanceState
    = Waiting
    | Running
    | Completed
    deriving (Eq, Show, Ord, Generic)

instance ToJSON InstanceState
instance FromJSON InstanceState

data InstanceSummary
    = InstanceSummary
        { _name :: Text
        , _players :: [PlayerInfo]
        , _state :: InstanceState
        , _maxPlayers :: Maybe Int
        }
    deriving (Eq, Show, Ord, Generic)

instance ToJSON InstanceSummary
instance FromJSON InstanceSummary

data ServerRequest a =
    ServerRequest
        { _playerKey :: String
        , _requestData :: a
        }
    deriving (Eq, Show, Ord, Generic)

instance ToJSON a => ToJSON (ServerRequest a)
instance FromJSON a => FromJSON (ServerRequest a)

data NewGameParams
    = NewGameParams
        { _name :: Text
        , _maxPlayers :: Int
        }
    deriving (Eq, Show, Ord, Generic)

instance ToJSON NewGameParams
instance FromJSON NewGameParams

data RequestData
    = ListLobbies
    | NewLobby PlayerInfo NewGameParams
    | JoinLobby String PlayerInfo
    | LeaveLobby String
    | StartGame String
    | GetGameState String
    | GameAction String Action
    deriving (Eq, Show, Ord, Generic)

instance ToJSON RequestData
instance FromJSON RequestData

data ServerResponse
    = ErrorResponse String
    | OkResponse Value
    deriving (Eq, Show, Generic)

instance ToJSON ServerResponse
instance FromJSON ServerResponse

data ServerState =
    ServerState
        { _instances :: Map String Instance
        }
    deriving (Eq, Show, Ord)

mkRecords' ''PlayerInfo
mkRecords' ''RunningGame
mkRecords' ''NewGameParams
mkRecords' ''Instance
mkRecords' ''InstanceDetails
mkRecords' ''InstanceView
mkRecords' ''InstanceViewDetails
mkRecords' ''ServerState
mkRecords' ''ServerRequest
