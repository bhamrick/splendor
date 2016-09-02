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
import Components.NewGameParams as NewGameParams

import Splendor.Types
import Splendor.Rules

data ActionSelection
    = TakeChipsSelection (Array Color)
    | CardSelection CardId
    | TopDeckSelection Int
    | NobleSelection NobleId
    | DiscardSelection (Map ChipType Int)

instance eqActionSelection :: Eq ActionSelection where
    eq (TakeChipsSelection cs1) (TakeChipsSelection cs2) = eq cs1 cs2
    eq (CardSelection cid1) (CardSelection cid2) = eq cid1 cid2
    eq (TopDeckSelection tier1) (TopDeckSelection tier2) = eq tier1 tier2
    eq (NobleSelection nid1) (NobleSelection nid2) = eq nid1 nid2
    eq (DiscardSelection chips1) (DiscardSelection chips2) = eq chips1 chips2
    eq _ _ = false

type ClientState =
    { clientKey :: String
    , playerInfo :: PlayerInfo
    , newGameParams :: NewGameParams
    , instanceList :: StrMap InstanceSummary
    , currentLobbyKey :: Maybe String
    , currentInstance :: Maybe InstanceView
    , currentSelection :: Maybe ActionSelection
    }

_playerInfo :: forall a b r. Lens { playerInfo :: a | r } { playerInfo :: b | r } a b
_playerInfo = lens _.playerInfo (_ { playerInfo = _ })

_newGameParams :: forall a b r. Lens { newGameParams :: a | r } { newGameParams :: b | r } a b
_newGameParams = lens _.newGameParams (_ { newGameParams = _ })

data ClientAction
    = OnPlayerInfo PlayerInfo.PlayerInfoAction
    | OnNewGameParams NewGameParams.NewGameParamsAction
    | NewLobbyAction
    | JoinLobbyAction String
    | LeaveLobbyAction
    | StartGameAction
    | ClearSelection
    | SelectChip Color
    | SelectCard CardId
    | SelectTopDeck Int
    | SelectAvailableNoble NobleId
    | AddToDiscard ChipType
    | RemoveFromDiscard ChipType
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
        , newGameParams: NewGameParams.defaultParams
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
                Just (InstanceView { details: CompletedInstanceView _ }) -> pure unit
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
newGameParamsSpec = T.focusState _newGameParams NewGameParams.spec

render :: T.Render ClientState _ _
render dispatch _ state _ =
    case state.currentInstance of
        Nothing ->
            case state.currentLobbyKey of
                Nothing ->
                    [ R.div'
                        [ R.div' $ (view T._render pInfoSpec) (dispatch <<< OnPlayerInfo)  [] state []
                        , R.div
                            [ RP.className "instanceList" ]
                            (renderInstanceList dispatch [] state.instanceList [])
                        , R.div
                            [ RP.className "newGameArea" ]
                            [ R.div [] $
                                (view T._render newGameParamsSpec) (dispatch <<< OnNewGameParams) [] state []
                            , R.button
                                [ RP.onClick \_ -> dispatch NewLobbyAction
                                ]
                                [ R.text "New Game"
                                ]
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
        Just (InstanceView inst) ->
            case inst.details of
                WaitingInstanceView wiv ->
                    [ R.div
                        [ RP.className "lobbyName" ]
                        [ R.text inst.name
                        , R.text $
                            " ("
                            <> (show $ Array.length wiv.waitingPlayers)
                            <> "/"
                            <> (show $ wiv.maxPlayers)
                            <> ")"
                        ]
                    , R.div
                        [ RP.className "lobbyPlayers" ]
                        (foldMap (\(PlayerInfo p) ->
                            [ R.div
                                [ RP.className "instancePlayer" ]
                                [ R.text (shortenName p.displayName) ]
                            ]) wiv.waitingPlayers
                        )
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
                        (renderGameView state.currentSelection dispatch [] (riv.runningGame) [])
                    ]
                CompletedInstanceView civ ->
                    [ R.div
                        [ RP.className "gameView"
                        ]
                        (renderCompletedGame civ.result dispatch [] civ.completedGame [])
                    ]

shortenName :: String -> String
shortenName n =
    if String.length n > 20
    then String.take 17 n <> "..."
    else n

renderGameView :: Maybe ActionSelection -> T.Render (RunningGame GameView) _ _
renderGameView selection dispatch p (RunningGame rg) _ =
    case rg.gameState of
        GameView gv ->
            [ R.div
                [ RP.className "wrapper" ]
                [ R.div
                    [ RP.className "board" ]
                    [ R.div
                        [ RP.className "actionRequestText" ]
                        [ R.text (actionRequestText gv.currentRequest rg.players)
                        ]
                    , R.table
                        [ RP.className "boardSupply" ]
                        [ R.tbody []
                            [ R.tr []
                                [ R.td []
                                    [ R.table []
                                        [ R.tbody []
                                            [ R.tr
                                                [ RP.className "availableChips" ]
                                                (renderAvailableChips selection dispatch p gv.availableChips [])
                                            , R.tr
                                                [ RP.className "discardChips" ]
                                                (renderDiscardChips selection dispatch p unit [])
                                            ]
                                        ]
                                    ]
                                , R.td
                                    [ RP.className "actionArea" ]
                                    (renderActionButtons selection dispatch p (GameView gv) [])
                                ]
                            ]
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
                                (let name = (fromMaybe "Player" ((\(PlayerInfo pi) -> shortenName pi.displayName) <$> Map.lookup gv.playerPosition rg.players))
                                in renderPlayerState name selection dispatch p gv.playerState [])
                            ] <>
                            Array.zipWith (\idx opp ->
                                R.tr
                                    [ RP.className "playerRow" ]
                                    (let name = (fromMaybe "Opponent" ((\(PlayerInfo pi) -> shortenName pi.displayName) <$> Map.lookup ((gv.playerPosition + 1 + idx) `mod` gv.numPlayers) rg.players))
                                    in renderPlayerView name dispatch [] opp [])
                            ) (Array.range 0 (Array.length gv.opponentViews - 1)) gv.opponentViews)
                        ]
                    ]
                ]
            , R.div
                [ RP.className "actionLogWrapper" ]
                [ R.textarea
                    [ RP.className "actionLog"
                    , RP.value $
                        String.joinWith "\n" $
                            map (\(Tuple idx action) ->
                                (case Map.lookup idx rg.players of
                                    Just (PlayerInfo pi) -> shortenName pi.displayName
                                    _ -> "Player " <> show idx
                                ) <> " " <> actionSummaryLine action
                            ) (Array.reverse gv.actionLog)
                    , RP.readOnly "true"
                    ]
                    []
                ]
            ]

actionSummaryLine :: ActionSummary -> String
actionSummaryLine action =
    case action of
        Took3 colors ->
            "took chips: " <> (String.joinWith ", " (map colorString colors))
        Took2 color ->
            "took two " <> colorString color <> " chips."
        Reserved card ->
            "reserved a card: " <> cardString card
        ReservedTop tier ->
            "reserved the top tier " <> show tier <> " card."
        Bought card ->
            "bought a card: " <> cardString card
        Discarded chips ->
            "discarded chips: " <> chipMapString chips
        GainedNoble noble ->
            "gained the noble " <> nobleString noble
    where
    colorString color =
        case color of
            Red -> "red"
            Green -> "green"
            White -> "white"
            Black -> "black"
            Blue -> "blue"
    chipString ctype =
        case ctype of
            Basic color -> colorString color
            Gold -> "gold"
    cardString (Card c) =
        show c.points <> " point "
        <> colorString c.color
        <> " costing "
        <> colorMapString c.cost
    nobleString (Noble n) =
        "requiring "
        <> colorMapString n.requirement
    colorMapString colors =
        String.joinWith ", " <<< Array.fromFoldable $
            map (\(Tuple color n) ->
                show n <> " " <> colorString color
            ) (Map.toList colors)
    chipMapString chips =
        String.joinWith ", " <<< Array.fromFoldable $
            map (\(Tuple chip n) ->
                show n <> " " <> chipString chip
            ) (Map.toList chips)

renderCompletedGame :: GameResult -> T.Render (RunningGame GameState) _ _
renderCompletedGame (GameWinners winners) dispatch p (RunningGame rg) _ =
    case rg.gameState of
        GameState gs ->
            [ R.div
                [ RP.className "wrapper" ]
                [ R.div
                    [ RP.className "board" ]
                    [ R.div
                        [ RP.className "winnersText" ]
                        [ R.text (String.joinWith " and " (map (\idx ->
                            case Map.lookup idx rg.players of
                                Nothing -> "Player " <> show idx
                                Just (PlayerInfo pi) -> shortenName pi.displayName
                            ) winners) <> " won.")
                        , R.button
                            [ RP.onClick \_ -> dispatch LeaveLobbyAction
                            ]
                            [ R.text "Leave" ]
                        ]
                    , R.table
                        [ RP.className "boardSupply" ]
                        [ R.tbody []
                            [ R.tr []
                                [ R.td []
                                    [ R.table []
                                        [ R.tbody []
                                            [ R.tr
                                                [ RP.className "availableChips" ]
                                                (renderAvailableChips Nothing dispatch p gs.availableChips [])
                                            , R.tr
                                                [ RP.className "discardChips" ]
                                                (renderDiscardChips Nothing dispatch p unit [])
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    , R.div
                        [ RP.className "availableNobles" ]
                        (renderAvailableNobles Nothing dispatch p gs.availableNobles [])
                    , R.div
                        [ RP.className "tierView" ]
                        (renderTierView Nothing 3 dispatch p (viewTier gs.tier3State) [])
                    , R.div
                        [ RP.className "tierView" ]
                        (renderTierView Nothing 2 dispatch p (viewTier gs.tier2State) [])
                    , R.div
                        [ RP.className "tierView" ]
                        (renderTierView Nothing 1 dispatch p (viewTier gs.tier1State) [])
                    ]
                , R.div
                    [ RP.className "players" ]
                    [ R.table
                        [ RP.className "playerBoards" ]
                        [ R.tbody []
                            (Array.zipWith (\idx player ->
                                R.tr
                                    [ RP.className "playerRow" ]
                                    (let name = fromMaybe ("Player " <> show idx) ((\(PlayerInfo pi) -> shortenName pi.displayName) <$> Map.lookup idx rg.players)
                                    in renderPlayerState name Nothing dispatch [] player [])
                            ) (Array.range 0 (Array.length gs.playerStates - 1)) gs.playerStates)
                        ]
                    ]
                ]
            , R.div
                [ RP.className "actionLogWrapper" ]
                [ R.textarea
                    [ RP.className "actionLog"
                    , RP.value $
                        String.joinWith "\n" $
                            map (\(Tuple idx action) ->
                                (case Map.lookup idx rg.players of
                                    Just (PlayerInfo pi) -> shortenName pi.displayName
                                    _ -> "Player " <> show idx
                                ) <> " " <> actionSummaryLine action
                            ) (Array.reverse gs.actionLog)
                    , RP.readOnly "true"
                    ]
                    []
                ]
            ]
    where
    viewTier (TierState ts) =
        TierView
            { availableCards: ts.availableCards
            , deckCount: Array.length ts.tierDeck
            }

renderInstanceList :: T.Render (StrMap InstanceSummary) _ _
renderInstanceList dispatch _ insts _ =
    foldMap subrender (List.filter (\(Tuple _ (InstanceSummary is)) -> is.state == Waiting) instList)
    <> foldMap subrender (List.filter (\(Tuple _ (InstanceSummary is)) -> is.state == Running) instList)
    <> foldMap subrender (List.filter (\(Tuple _ (InstanceSummary is)) -> is.state == Completed) instList)
    where
    subrender instanceData = renderInstanceSummary dispatch [] instanceData []
    instList = StrMap.toList insts

renderInstanceSummary :: T.Render (Tuple String InstanceSummary) _ _
renderInstanceSummary dispatch _ (Tuple instKey (InstanceSummary is)) _ =
    [ R.div
        [ RP.className "instanceSummary" ]
        [ R.div
            [ RP.className "instanceName" ]
            [ R.text is.name ]
        , R.div
            [ RP.className "instanceState" ]
            [ R.text (case is.state of
                Waiting -> "Waiting ("
                    <> (show $ Array.length (is.players))
                    <> "/"
                    <> (fromMaybe "?" $ show <$> is.maxPlayers)
                    <> ")"
                Running -> "Running"
                Completed -> "Completed"
            ) ]
        , R.div
            [ RP.className "instancePlayers" ]
            (map (\(PlayerInfo pi) ->
                R.div
                    [ RP.className "instancePlayer" ]
                    [ R.text (shortenName pi.displayName) ]
                ) is.players
            )
        , R.div
            [ RP.className "instanceActions" ]
            [ R.button
                [ RP.onClick \_ -> dispatch (JoinLobbyAction instKey) ]
                [ R.text "Join" ]
            ]
        ]
    ]

actionRequestText :: ActionRequest -> Map Int PlayerInfo -> String
actionRequestText (ActionRequest ar) players =
    "Waiting for "
    <> (fromMaybe "Unknown Player" ((\(PlayerInfo pi) -> shortenName pi.displayName) <$> Map.lookup ar.player players))
    <> (case ar.type_ of
        TurnRequest -> " to take their turn."
        DiscardChipRequest n -> " to discard " <> (show n) <> " chips."
        SelectNobleRequest -> " to select a noble to gain."
    )

renderActionButtons :: Maybe ActionSelection -> T.Render GameView _ _
renderActionButtons selection dispatch p (GameView gv) _ =
    Array.mapMaybe (\(Tuple text action) ->
        if isLegalAction (GameView gv) action
        then Just $
            R.button
                [ RP.onClick \_ -> dispatch (DoGameAction action) ]
                [ R.text text ]
        else Nothing
    ) (case selection of
        Nothing -> []
        Just (TakeChipsSelection colors) ->
            (if Array.length colors <= 3
            then [Tuple "Take Chips" (Take3 (Array.index colors 0) (Array.index colors 1) (Array.index colors 2))]
            else []
            ) <> (case Array.uncons colors of
                Just { head: c, tail: rest } ->
                    if Array.null rest
                    then [Tuple "Take Two" (Take2 c)]
                    else []
                Nothing -> []
            )
        Just (CardSelection cid) ->
            [ Tuple "Buy" (Buy cid)
            , Tuple "Reserve" (Reserve cid)
            ]
        Just (TopDeckSelection tier) ->
            [ Tuple "Reserve Top" (ReserveTop tier) ]
        Just (NobleSelection nid) ->
            [ Tuple "Select Noble" (SelectNoble nid) ]
        Just (DiscardSelection discards) ->
            [ Tuple "Confirm Discards" (Discard discards) ]
    )

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
    map (\ctype -> R.td []
        [ R.div
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
        ]
    ) [Basic Red, Basic Green, Basic Blue, Basic White, Basic Black, Gold]
    where
    chipNumber ctype chips =
        fromMaybe 0 (Map.lookup ctype chips)
    chipColorClass ctype =
        case ctype of
            Basic color -> colorClass color
            Gold -> "gold"

renderDiscardChips :: Maybe ActionSelection -> T.Render Unit _ _
renderDiscardChips selection dispatch p _ _ =
    let
    discardChips = case selection of
        Just (DiscardSelection chips) -> chips
        _ -> Map.empty
    in
    map (\ctype -> R.td []
        [ R.div
            [ RP.className "chipPlaceholder" ]
            (if chipNumber ctype discardChips > 0
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
                        [ RP.onClick \_ -> dispatch (RemoveFromDiscard ctype) ]))
                        [ R.text (show $ chipNumber ctype discardChips)
                        ]
                    ]
                else []
            )
        ]
    ) [Basic Red, Basic Green, Basic Blue, Basic White, Basic Black, Gold]
    where
    chipNumber ctype chips =
        fromMaybe 0 (Map.lookup ctype chips)
    chipColorClass ctype =
        case ctype of
            Basic color -> colorClass color
            Gold -> "gold"

renderPlayerState :: String -> Maybe ActionSelection -> T.Render PlayerState _ _
renderPlayerState name selection dispatch p (PlayerState ps) _ =
    let
    chipsToRender =
        case selection of
            Just (DiscardSelection discards) ->
                foldl (\m (Tuple ctype nDiscarded) ->
                    Map.alter (\maybeN ->
                        let
                        n = fromMaybe 0 maybeN
                        in
                        if n <= nDiscarded
                        then Nothing
                        else Just (n - nDiscarded)
                    ) ctype m
                ) ps.heldChips (Map.toList discards)
            _ -> ps.heldChips
    in
    [ R.td
        []
        [ R.div
            [ RP.className "playerName" ]
            [ R.text name ]
        , R.table []
            [ R.tbody []
                [ R.tr
                    [ RP.className "cardRow" ]
                    (map (\color -> R.td
                        [ RP.className "ownedCards" ]
                        (let
                        matchingCards = Array.reverse $ Array.filter (\(Card c) -> c.color == color) ps.ownedCards
                        in
                        if Array.null matchingCards
                        then []
                        else
                            [ R.div
                                [ RP.className "cardStack" ]
                                (map (\card ->
                                    R.div
                                        [ RP.className "card"
                                        ]
                                        (renderCard dispatch p card [])
                                    ) matchingCards
                                )
                            ]
                        )
                    ) [Red, Green, Blue, White, Black])
                , R.tr
                    [ RP.className "chipRow"
                    ]
                    (map (\ctype ->
                        R.td
                            [ RP.className "ownedChips"
                            ]
                            (
                            let
                            nChips = chipNumber ctype chipsToRender
                            in
                            if nChips > 0
                                then
                                    [ R.div
                                        [ RP.className (String.joinWith " " ["chip", chipClass ctype])
                                        , RP.onClick \_ -> dispatch (AddToDiscard ctype)
                                        ]
                                        [ R.text (show nChips)
                                        ]
                                    ]
                                else []
                            )
                        ) [Basic Red, Basic Green, Basic Blue, Basic White, Basic Black, Gold]
                    )
                ]
            ]
        ]
    , R.td
        [ RP.className "reservedCards"
        ]
        ([ R.div 
            [ RP.className "currentVP" ]
            [ R.text ("Current VP: " <> show ps.currentVP) ]
        ]
        <> map (\(Card c) ->
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
    ]
    where
    chipNumber ctype chips =
        fromMaybe 0 (Map.lookup ctype chips)

renderPlayerView :: String -> T.Render PlayerView _ _
renderPlayerView name dispatch p (PlayerView pv) _ =
    [ R.td
        []
        [ R.div
            [ RP.className "playerName" ]
            [ R.text name ]
        , R.table []
            [ R.tbody []
                [ R.tr
                    [ RP.className "cardRow" ]
                    (map (\color -> R.td
                        [ RP.className "ownedCards"
                        ]
                        (let
                        matchingCards = Array.reverse $ Array.filter (\(Card c) -> c.color == color) pv.ownedCards
                        in
                        if Array.null matchingCards
                        then []
                        else
                            [ R.div
                                [ RP.className "cardStack" ]
                                (map (\card ->
                                    R.div
                                        [ RP.className "card"
                                        ]
                                        (renderCard dispatch p card [])
                                    ) (Array.reverse $ Array.filter (\(Card c) -> c.color == color) pv.ownedCards)
                                )
                            ]
                        )
                    ) [Red, Green, Blue, White, Black])
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
            ]
        ]
    , R.td
        [ RP.className "reservedCards"
        ]
        ([ R.div
            [ RP.className "currentVP" ]
            [ R.text ("Current VP: " <> show pv.currentVP) ]
        ]
        <> replicate pv.reservedCardCount (
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
        OnNewGameParams a' -> do
            (view T._performAction newGameParamsSpec) a' p s
        NewLobbyAction -> do
            newLobbyKey <- lift $ makeRequest (ServerRequest
                { playerKey: s.clientKey
                , requestData: NewLobby s.playerInfo s.newGameParams
                })
            void $ T.cotransform (\state -> state { currentLobbyKey = newLobbyKey })
        JoinLobbyAction lobbyKey -> do
            (dat :: Maybe Json) <- lift $ makeRequest (ServerRequest
                { playerKey: s.clientKey
                , requestData: JoinLobby lobbyKey s.playerInfo
                })
            case dat of
                Nothing -> pure unit
                Just _ -> do
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
            case s.currentInstance of
                Just (InstanceView
                        { details: RunningInstanceView
                            { runningGame: RunningGame
                                { gameState: GameView
                                    { playerPosition: idx
                                    , currentRequest: ActionRequest ar
                                    }
                                }
                            }
                        }) ->
                    if ar.player == idx
                    then case ar.type_ of
                        TurnRequest ->
                            case s.currentSelection of
                                Just (TakeChipsSelection selectedColors) ->
                                    if any (\c -> c == color) selectedColors
                                        then do
                                            void $ T.cotransform (\state -> state { currentSelection = Just $ TakeChipsSelection (Array.filter (\c -> c /= color) selectedColors) })
                                        else do
                                            void $ T.cotransform (\state -> state { currentSelection = Just $ TakeChipsSelection (Array.snoc selectedColors color) })
                                _ -> do
                                    void $ T.cotransform (\state -> state { currentSelection = Just $ TakeChipsSelection [color] })
                        _ -> pure unit
                    else pure unit
                _ -> pure unit
        SelectCard cardId -> do
            case s.currentInstance of
                Just (InstanceView
                    { details: RunningInstanceView
                        { runningGame: RunningGame
                            { gameState: GameView
                                { playerPosition: idx
                                , currentRequest: ActionRequest ar
                                }
                            }
                        }
                    }) ->
                    if ar.player == idx
                    then case ar.type_ of
                        TurnRequest ->
                            if s.currentSelection == Just (CardSelection cardId)
                                then void $ T.cotransform (\state -> state { currentSelection = Nothing })
                                else void $ T.cotransform (\state -> state { currentSelection = Just $ CardSelection cardId })
                        _ -> pure unit
                    else pure unit
                _ -> pure unit
        SelectTopDeck tier -> do
            case s.currentInstance of
                Just (InstanceView
                    { details: RunningInstanceView
                        { runningGame: RunningGame
                            { gameState: GameView
                                { playerPosition: idx
                                , currentRequest: ActionRequest ar
                                }
                            }
                        }
                    }) ->
                    if ar.player == idx
                    then case ar.type_ of
                        TurnRequest ->
                            if s.currentSelection == Just (TopDeckSelection tier)
                                then void $ T.cotransform (\state -> state { currentSelection = Nothing })
                                else void $ T.cotransform (\state -> state { currentSelection = Just $ TopDeckSelection tier })
                        _ -> pure unit
                    else pure unit
                _ -> pure unit
        SelectAvailableNoble nid -> do
            case s.currentInstance of
                Just (InstanceView
                    { details: RunningInstanceView
                        { runningGame: RunningGame
                            { gameState: GameView
                                { playerPosition: idx
                                , currentRequest: ActionRequest ar
                                }
                            }
                        }
                    }) ->
                    if ar.player == idx
                    then case ar.type_ of
                        SelectNobleRequest ->
                            if s.currentSelection == Just (NobleSelection nid)
                                then void $ T.cotransform (\state -> state { currentSelection = Nothing })
                                else void $ T.cotransform (\state -> state { currentSelection = Just (NobleSelection nid) })
                        _ -> pure unit
                    else pure unit
                _ -> pure unit
        AddToDiscard ctype -> do
            case s.currentInstance of
                Just (InstanceView
                    { details: RunningInstanceView
                        { runningGame: RunningGame
                            { gameState: GameView
                                { playerPosition: idx
                                , currentRequest: ActionRequest ar
                                }
                            }
                        }
                    }) ->
                    if ar.player == idx
                    then case ar.type_ of
                        DiscardChipRequest _ ->
                            case s.currentSelection of
                                Just (DiscardSelection chips) -> do
                                    void $ T.cotransform (\state -> state { currentSelection = Just (DiscardSelection (Map.alter (\n -> Just $ 1 + fromMaybe 0 n) ctype chips)) })
                                _ -> void $ T.cotransform (\state -> state { currentSelection = Just (DiscardSelection (Map.singleton ctype 1)) })
                        _ -> pure unit
                    else pure unit
                _ -> pure unit
        RemoveFromDiscard ctype -> do
            case s.currentInstance of
                Just (InstanceView
                    { details: RunningInstanceView
                        { runningGame: RunningGame
                            { gameState: GameView
                                { playerPosition: idx
                                , currentRequest: ActionRequest ar
                                }
                            }
                        }
                    }) ->
                    if ar.player == idx
                    then case ar.type_ of
                        DiscardChipRequest _ ->
                            case s.currentSelection of
                                Just (DiscardSelection chips) -> do
                                    void $ T.cotransform (\state -> state { currentSelection = Just (DiscardSelection (Map.alter (\n -> let n' = fromMaybe 0 n - 1 in if n' <= 0 then Nothing else Just n') ctype chips)) })
                                    void $ T.cotransform (\state -> if state.currentSelection == Just (DiscardSelection Map.empty)
                                        then state { currentSelection = Nothing }
                                        else state)
                                _ -> pure unit
                        _ -> pure unit
                    else pure unit
                _ -> pure unit
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
