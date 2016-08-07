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
import Data.Array as Array
import Data.Either
import Data.Foldable
import Data.Int as Int
import Data.Lens
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe
import Data.Generic
import Data.String as String
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Tuple
import Data.Unfoldable

import Network.HTTP.Affjax
import Network.HTTP.StatusCode

import Components.PlayerInfo as PlayerInfo

import Splendor.Types

data ActionSelection
    = TakeChipsSelection (Array Color)
    | CardSelection CardId
    | TopDeckSelection Int
    | NobleSelection NobleId

derive instance genericActionSelection :: Generic ActionSelection

instance eqActionSelection :: Eq ActionSelection where
    eq = gEq

type ClientState =
    { clientKey :: String
    , playerInfo :: PlayerInfo
    , instanceList :: StrMap InstanceSummary
    , currentLobbyKey :: Maybe String
    , currentInstance :: Maybe InstanceView
    , currentSelection :: Maybe ActionSelection
    }

_playerInfo :: forall a b r. Lens { playerInfo :: a | r } { playerInfo :: b | r } a b
_playerInfo = lens _.playerInfo (_ { playerInfo = _ })

data ClientAction
    = OnPlayerInfo PlayerInfo.PlayerInfoAction
    | NewLobbyAction
    | JoinLobbyAction String
    | LeaveLobbyAction
    | StartGameAction
    | ClearSelection
    | SelectChip Color
    | SelectCard CardId
    | SelectTopDeck Int
    | SelectAvailableNoble NobleId
    | DoGameAction Action

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
        , currentSelection: Nothing
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
                        (renderGameView state.currentSelection dispatch p (riv.runningGame) [])
                    ]
                CompletedInstanceView civ ->
                    [ R.text "Completed game placeholder" ]

renderGameView :: Maybe ActionSelection -> T.Render (RunningGame GameView) _ _
renderGameView selection dispatch p (RunningGame rg) _ =
    case rg.gameState of
        GameView gv ->
            [ R.div
                [ RP.className "board" ]
                [ R.div
                    [ RP.className "actionRequestText" ]
                    [ R.text (actionRequestText gv.currentRequest rg.players)
                    ]
                , R.div
                    [ RP.className "boardSupply" ]
                    [ R.div
                        [ RP.className "availableChips" ]
                        (renderAvailableChips selection dispatch p gv.availableChips [])
                    , R.div
                        [ RP.className "actionArea" ]
                        (renderActionButtons selection dispatch p (GameView gv) [])
                    ]
                , R.div [ RP.className "availableNobles" ]
                    (renderAvailableNobles selection dispatch p gv.availableNobles [])
                , R.div
                    [ RP.className "tierView" ]
                    (renderTierView selection 3 dispatch p gv.tier3View [])
                , R.div
                    [ RP.className "tierView" ]
                    (renderTierView selection 2 dispatch p gv.tier2View [])
                , R.div
                    [ RP.className "tierView" ]
                    (renderTierView selection 1 dispatch p gv.tier1View [])
                ]
            , R.div
                [ RP.className "players" ]
                [ R.table
                    [ RP.className "playerBoards" ]
                    [ R.tbody []
                        ([ R.tr
                            [ RP.className "playerRow" ]
                            [ R.td
                                [ RP.className "playerName" ]
                                [ R.text (fromMaybe "Player" ((\(PlayerInfo pi) -> pi.displayName) <$> Map.lookup gv.playerPosition rg.players))
                                ]
                            , R.td
                                [ RP.className "myBoard" ]
                                [ R.table []
                                    [ R.tbody []
                                        (renderPlayerState selection dispatch p gv.playerState [])
                                    ]
                                ]
                            ]
                        ] <>
                        Array.zipWith (\idx opp ->
                            R.tr
                                [ RP.className "playerRow" ]
                                [ R.td
                                    [ RP.className "playerName" ]
                                    [ R.text (fromMaybe "Opponent" ((\(PlayerInfo pi) -> pi.displayName) <$> Map.lookup ((gv.playerPosition + 1 + idx)`mod` gv.numPlayers) rg.players))
                                    ]
                                , R.td
                                    [ RP.className "oppBoard" ]
                                    [ R.table []
                                        [ R.tbody []
                                            (renderPlayerView dispatch p opp [])
                                        ]
                                    ]
                                ]
                        ) (Array.range 0 (Array.length gv.opponentViews - 1)) gv.opponentViews)
                    ]
                ]
            ]

actionRequestText :: ActionRequest -> Map Int PlayerInfo -> String
actionRequestText (ActionRequest ar) players =
    "Waiting for "
    <> (fromMaybe "Unknown Player" ((\(PlayerInfo pi) -> pi.displayName) <$> Map.lookup ar.player players))
    <> (case ar.type_ of
        TurnRequest -> " to take their turn."
        DiscardChipRequest n -> " to discard " <> (show n) <> " chips."
        SelectNobleRequest -> " to select a noble to gain."
    )

renderActionButtons :: Maybe ActionSelection -> T.Render GameView _ _
renderActionButtons selection dispatch p (GameView gv) _ =
    case selection of
        Nothing -> []
        Just (TakeChipsSelection colors) ->
            (if Array.length colors == min 3 numAvailableChipTypes
            then
                [ R.button
                    [ RP.onClick \_ -> dispatch (DoGameAction (Take3 (Array.index colors 0) (Array.index colors 1) (Array.index colors 2)))
                    ]
                    [ R.text "Take chips"
                    ]
                ]
            else [])
            <> (case Array.uncons colors of
                Just { head: c, tail: rest } ->
                    if Array.null rest && fromMaybe 0 (Map.lookup (Basic c) gv.availableChips) >= 4
                    then
                        [ R.button
                            [ RP.onClick \_ -> dispatch (DoGameAction (Take2 c))
                            ]
                            [ R.text "Take two"
                            ]
                        ]
                    else []
                _ -> [])
        Just (CardSelection cardId) ->
            -- TODO: Check legality of reserve/buy 
            [ R.button
                [ RP.onClick \_ -> dispatch (DoGameAction (Buy cardId))
                ]
                [ R.text "Buy"
                ]
            , R.button
                [ RP.onClick \_ -> dispatch (DoGameAction (Reserve cardId))
                ]
                [ R.text "Reserve"
                ]
            ]
        Just (TopDeckSelection tier) ->
            [ R.button
                [ RP.onClick \_ -> dispatch (DoGameAction (ReserveTop tier))
                ]
                [ R.text "Reserve Top Card"
                ]
            ]
        Just (NobleSelection nid) ->    
            [ R.button
                [ RP.onClick \_ -> dispatch (DoGameAction (SelectNoble nid))
                ]
                [ R.text "Select"
                ]
            ]
    where
    numAvailableChipTypes = List.length <<< List.filter (\(Tuple _ n) -> n > 0) $ Map.toList gv.availableChips

renderTierView :: Maybe ActionSelection -> Int -> T.Render TierView _ _
renderTierView selection tier dispatch p (TierView tv) _ =
    map (\(Card c) ->
        R.div
            (let
            classes = if selection == Just (CardSelection c.id)
                then "selected card"
                else "card"
            in
            [ RP.className classes
            , RP.onClick \_ -> dispatch (SelectCard c.id)
            ])
            (renderCard dispatch p (Card c) [])
        ) tv.availableCards
    <> [ R.div
        (let
        classes = if selection == Just (TopDeckSelection tier)
            then "selected tierDeck"
            else "tierDeck"
        in
        [ RP.className classes
        , RP.onClick \_ -> dispatch (SelectTopDeck tier)
        ])
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

renderNoble :: T.Render Noble _ _
renderNoble dispatch p (Noble n) _ =
    [ R.div
        [ RP.className "nobleTop" ]
        [ R.text (show n.points) ]
    , R.div
        [ RP.className "nobleRequirement"
        ]
        (foldMap (\(Tuple c n) ->
            [ R.div
                [ RP.className (String.joinWith " " ["nobleRequirementComponent", colorClass c])
                ]
                [ R.text (show n)
                ]
            ]) (Map.toList n.requirement)
        )
    ]

renderAvailableNobles :: Maybe ActionSelection -> T.Render (Array Noble) _ _
renderAvailableNobles selection dispatch p nobles _ =
    map (\(Noble n) -> R.div
        (let
        classes = if selection == Just (NobleSelection n.id)
            then "selected noble"
            else "noble"
        in
        [ RP.className classes
        , RP.onClick \_ -> dispatch (SelectAvailableNoble n.id)
        ])
        (renderNoble dispatch p (Noble n) [])
    ) nobles

renderAvailableChips :: Maybe ActionSelection -> T.Render (Map ChipType Int) _ _
renderAvailableChips selection dispatch p chips _ =
    map (\ctype -> R.div
        [ RP.className "chipPlaceholder" ]
        (if chipNumber ctype chips > 0
            then
                [ R.div
                    (let
                    classes =
                        case selection of
                            Just (TakeChipsSelection colors) ->
                                if any (\c -> Basic c == ctype) colors
                                    then String.joinWith " " ["selected", "chip", chipColorClass ctype]
                                    else String.joinWith " " ["chip", chipColorClass ctype]
                            _ -> String.joinWith " " ["chip", chipColorClass ctype]
                    in
                    ([ RP.className classes
                    ] <>
                    case ctype of
                        Basic color -> [ RP.onClick \_ -> dispatch (SelectChip color) ]
                        Gold -> []
                    ))
                    [ R.text (show $ chipNumber ctype chips)
                    ]
                ]
            else []
        )
    ) [Basic Red, Basic Green, Basic Blue, Basic White, Basic Black, Gold]
    where
    chipNumber ctype chips =
        fromMaybe 0 (Map.lookup ctype chips)
    chipColorClass ctype =
        case ctype of
            Basic color -> colorClass color
            Gold -> "gold"

renderPlayerState :: Maybe ActionSelection -> T.Render PlayerState _ _
renderPlayerState selection dispatch p (PlayerState ps) _ =
    [ R.tr
        [ RP.className "cardRow"
        ]
        (map (\color -> R.td
            [ RP.className "ownedCards"
            ]
            (map (\card ->
                R.div
                    [ RP.className "card"
                    ]
                    (renderCard dispatch p card [])
                ) (Array.reverse $ Array.filter (\(Card c) -> c.color == color) ps.ownedCards)
            )
        ) [Red, Green, Blue, White, Black] <>
        [ R.td [] []
        , R.td
            [ RP.className "reservedCards"
            , RP.rowSpan "2"
            ]
            (map (\(Card c) ->
                R.div
                    (let
                    classes = if selection == Just (CardSelection c.id)
                        then "selected card"
                        else "card"
                    in
                    [ RP.className classes
                    , RP.onClick \_ -> dispatch (SelectCard c.id)
                    ])
                    (renderCard dispatch p (Card c) [])
                ) (Array.reverse ps.reservedCards)
            <> [ R.br [] [] ]
            <> map (\noble ->
                R.div
                    [ RP.className "noble"
                    ]
                    (renderNoble dispatch p noble [])
            ) ps.ownedNobles)
        ])
    , R.tr
        [ RP.className "chipRow"
        ]
        (map (\ctype ->
            R.td
                [ RP.className "ownedChips"
                ]
                (if chipNumber ctype ps.heldChips > 0
                    then
                        [ R.div
                            [ RP.className (String.joinWith " " ["chip", chipClass ctype])
                            ]
                            [ R.text (show $ chipNumber ctype ps.heldChips)
                            ]
                        ]
                    else []
                )
            ) [Basic Red, Basic Green, Basic Blue, Basic White, Basic Black, Gold]
        )
    ]
    where
    chipNumber ctype chips =
        fromMaybe 0 (Map.lookup ctype chips)

renderPlayerView :: T.Render PlayerView _ _
renderPlayerView dispatch p (PlayerView pv) _ =
    [ R.tr
        [ RP.className "cardRow"
        ]
        (map (\color -> R.td
            [ RP.className "ownedCards"
            ]
            (map (\card ->
                R.div
                    [ RP.className "card"
                    ]
                    (renderCard dispatch p card [])
                ) (Array.reverse $ Array.filter (\(Card c) -> c.color == color) pv.ownedCards)
            )
        ) [Red, Green, Blue, White, Black] <>
        [ R.td [] []
        , R.td
            [ RP.className "reservedCards"
            , RP.rowSpan "2"
            ]
            (replicate pv.reservedCardCount (
                R.div
                    [ RP.className "facedownCard"
                    ]
                    [ R.text "\x00a0" ]
                )
            <> [ R.br [] [] ]
            <> map (\noble ->
                R.div
                    [ RP.className "noble"
                    ]
                    (renderNoble dispatch p noble [])
            ) pv.ownedNobles)
        ])
    , R.tr
        [ RP.className "chipRow"
        ]
        (map (\ctype ->
            R.td
                [ RP.className "ownedChips"
                ]
                (if chipNumber ctype pv.heldChips > 0
                    then
                        [ R.div
                            [ RP.className (String.joinWith " " ["chip", chipClass ctype])
                            ]
                            [ R.text (show $ chipNumber ctype pv.heldChips)
                            ]
                        ]
                    else []
                )
            ) [Basic Red, Basic Green, Basic Blue, Basic White, Basic Black, Gold]
        )
    ]
    where
    chipNumber ctype chips =
        fromMaybe 0 (Map.lookup ctype chips)

chipClass :: ChipType -> String
chipClass ctype =
    case ctype of
        Basic c -> colorClass c
        Gold -> "gold"

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
        ClearSelection -> do
            void $ T.cotransform (\state -> state { currentSelection = Nothing })
        SelectChip color -> do
            case s.currentSelection of
                Just (TakeChipsSelection selectedColors) ->
                    if any (\c -> c == color) selectedColors
                        then do
                            void $ T.cotransform (\state -> state { currentSelection = Just $ TakeChipsSelection (Array.filter (\c -> c /= color) selectedColors) })
                        else do
                            void $ T.cotransform (\state -> state { currentSelection = Just $ TakeChipsSelection (Array.snoc selectedColors color) })
                _ -> do
                    void $ T.cotransform (\state -> state { currentSelection = Just $ TakeChipsSelection [color] })
        SelectCard cardId -> do
            if s.currentSelection == Just (CardSelection cardId)
                then void $ T.cotransform (\state -> state { currentSelection = Nothing })
                else void $ T.cotransform (\state -> state { currentSelection = Just $ CardSelection cardId })
        SelectTopDeck tier -> do
            if s.currentSelection == Just (TopDeckSelection tier)
                then void $ T.cotransform (\state -> state { currentSelection = Nothing })
                else void $ T.cotransform (\state -> state { currentSelection = Just $ TopDeckSelection tier })
        SelectAvailableNoble nid -> do
            if s.currentSelection == Just (NobleSelection nid)
                then void $ T.cotransform (\state -> state { currentSelection = Nothing })
                else void $ T.cotransform (\state -> state { currentSelection = Just (NobleSelection nid) })
        DoGameAction action -> do
            case s.currentLobbyKey of
                Nothing -> pure unit
                Just gameKey -> do
                    (dat :: Maybe Json) <- lift $ makeRequest (ServerRequest
                        { playerKey: s.clientKey
                        , requestData: GameAction gameKey action
                        })
                    case dat of
                        Nothing -> pure unit
                        Just _ -> do
                            void $ T.cotransform (\state -> state { currentSelection = Nothing })

spec :: T.Spec _ ClientState _ ClientAction
spec = T.simpleSpec performAction render
