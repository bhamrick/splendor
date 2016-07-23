module Components.Client where

import Prelude
import Thermite as T

import React as R
import React.DOM as R
import React.DOM.Props as RP

import Browser.LocalStorage
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Trans
import Data.Argonaut
import Data.Either
import Data.Foldable
import Data.Int as Int
import Data.Lens
import Data.Maybe
import Data.Generic
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Tuple

import Network.HTTP.Affjax
import Network.HTTP.StatusCode

import Components.PlayerInfo as PlayerInfo

import Splendor.Types

type ClientState =
    { clientKey :: String
    , playerInfo :: PlayerInfo
    , lobbyList :: StrMap LobbyView
    }

_playerInfo :: forall a b r. Lens { playerInfo :: a | r } { playerInfo :: b | r } a b
_playerInfo = lens _.playerInfo (_ { playerInfo = _ })

data ClientAction
    = OnPlayerInfo PlayerInfo.PlayerInfoAction
    | RefreshLobbies
    | NewLobbyAction
    | JoinLobbyAction String

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

initializeState :: forall e. Eff (storage :: STORAGE, random :: RANDOM | e) ClientState
initializeState = do
    storedKey <- localStorage.getItem clientKeyKey
    key <- case storedKey of
        Nothing -> do
            k <- newKey
            localStorage.setItem clientKeyKey k
            pure k
        Just k -> do
            pure k
    pInfo <- PlayerInfo.initializePlayerInfo
    pure $
        { clientKey: key
        , playerInfo: pInfo
        , lobbyList: StrMap.empty :: StrMap LobbyView
        }

-- Lifted specs for subcomponents
pInfoSpec = T.focusState _playerInfo PlayerInfo.spec

render :: T.Render ClientState _ _
render dispatch p state _ =
    [ R.div'
        [ R.p'
            [ R.text "Client Key: "
            , R.text $ state.clientKey
            ]
        , R.div' $ (view T._render pInfoSpec) (dispatch <<< OnPlayerInfo)  p state []
        , R.div' $ foldMap (\(Tuple lobbyKey lobbyView) ->
            [ R.text "Lobby: "
            , R.text lobbyKey
            , R.text $ gShow lobbyView
            ]) (StrMap.toList state.lobbyList)
        ]
    ]

performAction :: T.PerformAction _ ClientState _ ClientAction
performAction a p s =
    case a of
        OnPlayerInfo a' -> do
            (view T._performAction pInfoSpec) a' p s
        RefreshLobbies -> do
            dat <- lift $ do
                res <- post "/" (encodeJson (ServerRequest
                    { playerKey: s.clientKey
                    , requestData: ListLobbies
                    }))
                if res.status == StatusCode 200
                    then pure $ either (const Nothing) Just (decodeJson (res.response))
                    else pure Nothing
            case dat of
                Nothing -> pure unit
                Just lobbies -> do
                    void $ T.cotransform (\state -> state { lobbyList = lobbies })
        NewLobbyAction -> do
            pure unit
        JoinLobbyAction lobbyKey -> do
            pure unit

spec :: T.Spec _ ClientState _ ClientAction
spec = T.simpleSpec performAction render
