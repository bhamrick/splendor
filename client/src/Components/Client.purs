module Components.Client where

import Prelude
import Thermite as T

import React as R
import React.DOM as R
import React.DOM.Props as RP

import Browser.LocalStorage
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Random
import Control.Monad.Rec.Class
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
    , instanceList :: StrMap InstanceSummary
    , currentLobbyKey :: Maybe String
    , currentInstance :: Maybe InstanceView
    }

_playerInfo :: forall a b r. Lens { playerInfo :: a | r } { playerInfo :: b | r } a b
_playerInfo = lens _.playerInfo (_ { playerInfo = _ })

data ClientAction
    = OnPlayerInfo PlayerInfo.PlayerInfoAction
    | NewLobbyAction
    | JoinLobbyAction String
    | LeaveLobbyAction

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
        , instanceList: StrMap.empty :: StrMap InstanceSummary
        , currentLobbyKey: Nothing
        , currentInstance: Nothing
        }

makeRequest :: forall a b e. (EncodeJson a, DecodeJson b) => ServerRequest a -> Aff ( ajax :: AJAX | e ) (Maybe b)
makeRequest req = do
    res <- post "/" (encodeJson req)
    if res.status == StatusCode 200
        then case decodeJson (res.response) of
            Left _ -> pure Nothing
            Right (ErrorResponse _) -> pure Nothing
            Right (OkResponse dat) ->
                case decodeJson dat of
                    Left _ -> pure Nothing
                    Right val -> pure (Just val)
        else pure Nothing

backgroundWork :: forall e. R.ReactThis _ ClientState -> Aff ( ajax :: AJAX, console :: CONSOLE | e ) Unit
backgroundWork rthis = forever do
    refreshLobbies rthis
    refreshGame rthis
    later' 1000 (pure unit)

refreshLobbies :: forall e. R.ReactThis _ ClientState -> Aff _ Unit
refreshLobbies rthis = do
    s <- liftEff $ R.readState rthis
    dat <- makeRequest (ServerRequest
        { playerKey: s.clientKey
        , requestData: ListLobbies
        })
    liftEff $ log (show dat)
    case dat of
        Nothing -> pure unit
        Just instances -> do
            liftEff $ R.transformState rthis (\state -> state { instanceList = instances })

refreshGame :: forall e. R.ReactThis _ ClientState -> Aff _ Unit
refreshGame rthis = do
    s <- liftEff $ R.readState rthis
    case s.currentLobbyKey of
        Nothing -> pure unit
        Just lobbyKey ->
            case s.currentInstance of
                Just (CompletedInstanceView _) -> pure unit
                _ -> do
                    instState <- makeRequest (ServerRequest
                        { playerKey: s.clientKey
                        , requestData: GetGameState lobbyKey
                        })
                    case instState of
                        Just _ ->
                            liftEff $ R.transformState rthis (\state -> state { currentInstance = instState })
                        Nothing ->
                            liftEff $ R.transformState rthis (\state -> state { currentLobbyKey = Nothing, currentInstance = Nothing })

-- Lifted specs for subcomponents
pInfoSpec = T.focusState _playerInfo PlayerInfo.spec

render :: T.Render ClientState _ _
render dispatch p state _ =
    case state.currentInstance of
        Nothing ->
            case state.currentLobbyKey of
                Nothing ->
                    [ R.div'
                        [ R.p'
                            [ R.text "Client Key: "
                            , R.text $ state.clientKey
                            ]
                        , R.div' $ (view T._render pInfoSpec) (dispatch <<< OnPlayerInfo)  p state []
                        , R.div' $ foldMap (\(Tuple lobbyKey lobbyView) ->
                            [ R.div' $
                                [ R.text "Lobby: "
                                , R.text lobbyKey
                                , R.text $ gShow lobbyView
                                , R.button
                                    [ RP.onClick \_ -> dispatch (JoinLobbyAction lobbyKey)
                                    ]
                                    [ R.text "Join"
                                    ]
                                ]
                            ]) (StrMap.toList state.instanceList)
                        , R.button
                            [ RP.onClick \_ -> dispatch NewLobbyAction
                            ]
                            [ R.text "New Game"
                            ]
                        ]
                    ]
                Just lobbyKey ->
                    [ R.text "Attempting to join lobby: "
                    , R.text lobbyKey
                    , R.button
                        [ RP.onClick \_ -> dispatch LeaveLobbyAction
                        ]
                        [ R.text "Leave Game"
                        ]
                    ]
        Just inst ->
            case inst of
                WaitingInstanceView wiv ->
                    [ R.div' $
                        [ R.text "Waiting in lobby"
                        , R.button
                            [ RP.onClick \_ -> dispatch LeaveLobbyAction
                            ]
                            [ R.text "Leave Game"
                            ]
                        ]
                    , R.div' $
                        [ R.text "Players:" ]
                        <> foldMap (\(PlayerInfo p) ->
                            [ R.text " "
                            , R.text p.displayName
                            ]) wiv.waitingPlayers
                    ]
                RunningInstanceView riv ->
                    [ R.text "Running game placeholder" ]
                CompletedInstanceView civ ->
                    [ R.text "Completed game placeholder" ]

performAction :: T.PerformAction _ ClientState _ ClientAction
performAction a p s =
    case a of
        OnPlayerInfo a' -> do
            (view T._performAction pInfoSpec) a' p s
        NewLobbyAction -> do
            newLobbyKey <- lift $ makeRequest (ServerRequest
                { playerKey: s.clientKey
                , requestData: NewLobby s.playerInfo
                })
            void $ T.cotransform (\state -> state { currentLobbyKey = newLobbyKey })
        JoinLobbyAction lobbyKey -> do
            (dat :: Maybe Json) <- lift $ makeRequest (ServerRequest
                { playerKey: s.clientKey
                , requestData: JoinLobby lobbyKey s.playerInfo
                })
            case dat of
                Nothing -> pure unit
                Just _ -> void $ T.cotransform (\state -> state { currentLobbyKey = Just lobbyKey })
        LeaveLobbyAction -> do
            case s.currentLobbyKey of
                Nothing -> pure unit
                Just lobbyKey -> do
                    (dat :: Maybe Json) <- lift $ makeRequest (ServerRequest
                        { playerKey: s.clientKey
                        , requestData: LeaveLobby lobbyKey
                        })
                    case dat of
                        Nothing -> pure unit
                        Just _ -> void $ T.cotransform (\state -> state { currentLobbyKey = Nothing, currentInstance = Nothing })

spec :: T.Spec _ ClientState _ ClientAction
spec = T.simpleSpec performAction render
