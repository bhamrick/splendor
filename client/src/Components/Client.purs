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
import Data.Map as Map
import Data.Maybe
import Data.Generic
import Data.String as String
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
    | StartGameAction

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
                    [ R.text "Loading..."
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
                        ]
                    , R.div' $
                        [ R.text "Players:" ]
                        <> foldMap (\(PlayerInfo p) ->
                            [ R.text " "
                            , R.text p.displayName
                            ]) wiv.waitingPlayers
                    , R.button
                        [ RP.onClick \_ -> dispatch LeaveLobbyAction
                        ]
                        [ R.text "Leave Game"
                        ]
                    , R.button
                        [ RP.onClick \_ -> dispatch StartGameAction
                        ]
                        [ R.text "Start Game"
                        ]
                    ]
                RunningInstanceView riv ->
                    [ R.div
                        [ RP.className "gameView"
                        ]
                        (renderGameView dispatch p (riv.runningGame) [])
                    ]
                CompletedInstanceView civ ->
                    [ R.text "Completed game placeholder" ]

renderGameView :: T.Render (RunningGame GameView) _ _
renderGameView dispatch p (RunningGame rg) _ =
    case rg.gameState of
        GameView gv ->
            [ R.div
                [ RP.className "tierView" ]
                (renderTierView dispatch p gv.tier3View [])
            , R.div
                [ RP.className "tierView" ]
                (renderTierView dispatch p gv.tier2View [])
            , R.div
                [ RP.className "tierView" ]
                (renderTierView dispatch p gv.tier1View [])
            ]

renderTierView :: T.Render TierView _ _
renderTierView dispatch p (TierView tv) _ =
    map (\c ->
        R.div
            [ RP.className "card"
            ]
            (renderCard dispatch p c [])
        ) tv.availableCards
    <> [ R.div
        [ RP.className "tierDeck"
        ]
        [ R.text (show tv.deckCount)
        ]
    ]

renderCard :: T.Render Card _ _
renderCard dispatch p (Card c) _ =
    [ R.div
        [ RP.className (String.joinWith " " ["cardTop", colorClass c.color])
        ]
        if c.points > 0
            then [ R.text (show c.points) ]
            else [ R.text "\x00a0" ]
    , R.div
        [ RP.className "cardPrice"
        ]
        (foldMap (\(Tuple c n) ->
            [ R.div
                [ RP.className (String.joinWith " " ["cardPriceComponent", colorClass c])
                ]
                [ R.text (show n)
                ]
            ]) (Map.toList c.cost)
        )
    ]

colorClass :: Color -> String
colorClass c =
    case c of
        Red -> "red"
        Green -> "green"
        Blue -> "blue"
        White -> "white"
        Black -> "black"

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
            (_ :: Maybe Json) <- lift $ makeRequest (ServerRequest
                { playerKey: s.clientKey
                , requestData: JoinLobby lobbyKey s.playerInfo
                })
            void $ T.cotransform (\state -> state { currentLobbyKey = Just lobbyKey })
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
        StartGameAction -> do
            case s.currentLobbyKey of
                Nothing -> pure unit
                Just lobbyKey -> do
                    (_ :: Maybe Json) <- lift $ makeRequest (ServerRequest
                        { playerKey: s.clientKey
                        , requestData: StartGame lobbyKey
                        })
                    pure unit

spec :: T.Spec _ ClientState _ ClientAction
spec = T.simpleSpec performAction render
