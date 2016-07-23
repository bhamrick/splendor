module Components.Client where

import Prelude
import Thermite as T

import React as R
import React.DOM as R
import React.DOM.Props as RP

import Browser.LocalStorage
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Data.Maybe
import Data.Int as Int
import Data.Generic

import Data.Lens

import Components.PlayerInfo as PlayerInfo

import Splendor.Types

type ClientState =
    { clientKey :: String
    , playerInfo :: PlayerInfo
    }

_playerInfo :: forall a b r. Lens { playerInfo :: a | r } { playerInfo :: b | r } a b
_playerInfo = lens _.playerInfo (_ { playerInfo = _ })

data ClientAction =
    OnPlayerInfo PlayerInfo.PlayerInfoAction

newKey :: forall e. Eff (random :: RANDOM | e) String
newKey = go 40
    where
    go n =
        if n == 0
        then pure ""
        else do
            c <- randomInt 0 15
            rest <- go (n-1)
            pure $ (Int.toStringAs Int.hexadecimal c) <> rest

data StorageKey a = ClientKeyKey
derive instance genericStorageKey :: Generic (StorageKey a)

clientKeyKey :: StorageKey String
clientKeyKey = ClientKeyKey

basicState :: ClientState
basicState =
    { clientKey: ""
    , playerInfo: PlayerInfo
        { displayName: PlayerInfo.defaultName
        }
    }

initializeState :: forall e. Eff (storage :: STORAGE, random :: RANDOM | e) ClientState
initializeState = do
    storedKey <- localStorage.getItem clientKeyKey
    key <- case storedKey of
        Nothing -> do
            key <- newKey
            localStorage.setItem clientKeyKey key
            pure key
        Just key -> do
            pure key
    pInfo <- PlayerInfo.initializePlayerInfo
    pure $
        { clientKey: key
        , playerInfo: pInfo
        }

-- Lifted specs for subcomponents
pInfoSpec = T.focusState _playerInfo PlayerInfo.spec

render :: T.Render ClientState _ _
render dispatch p state _ =
    [ R.p'
        [ R.text "Client Key: "
        , R.text $ state.clientKey
        , R.div' $ (view T._render pInfoSpec) (dispatch <<< OnPlayerInfo)  p state []
        ]
    ]

performAction :: T.PerformAction _ ClientState _ ClientAction
performAction a p s =
    case a of
        OnPlayerInfo a' -> do
            (view T._performAction pInfoSpec) a' p s

spec :: T.Spec _ ClientState _ ClientAction
spec = T.simpleSpec performAction render
