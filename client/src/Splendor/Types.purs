module Splendor.Types where

import Prelude

import Data.Argonaut
import Data.Array as Array
import Data.Either
import Data.Foldable
import Data.Generic
import Data.Int as Int
import Data.Maybe
import Data.Map (Map)
import Data.Map as Map
import Data.StrMap as StrMap
import Data.String
import Data.Traversable
import Data.Tuple

newtype CardId = CardId Int

derive instance genericCardId :: Generic CardId

instance showCardId :: Show CardId where
    show = gShow

instance eqCardId :: Eq CardId where
    eq = gEq

instance ordCardId :: Ord CardId where
    compare = gCompare

instance decodeJsonCardId :: DecodeJson CardId where
    decodeJson j = CardId <$> decodeJson j

instance encodeJsonCardId :: EncodeJson CardId where
    encodeJson (CardId n) = encodeJson n

newtype NobleId = NobleId Int

derive instance genericNobleId :: Generic NobleId

instance showNobleId :: Show NobleId where
    show = gShow

instance eqNobleId :: Eq NobleId where
    eq = gEq

instance ordNobleId :: Ord NobleId where
    compare = gCompare

instance decodeJsonNobleId :: DecodeJson NobleId where
    decodeJson j = NobleId <$> decodeJson j

instance encodeJsonNobleId :: EncodeJson NobleId where
    encodeJson (NobleId n) = encodeJson n

class Keyable a where
    decodeKey :: String -> Either String a
    encodeKey :: a -> String

instance keyableInt :: Keyable Int where
    decodeKey s =
        case Int.fromString s of
            Nothing -> Left "Invalid integer"
            Just n -> Right n
    encodeKey = show

data Color
    = Red
    | Green
    | Blue
    | White
    | Black

derive instance genericColor :: Generic Color

instance showColor :: Show Color where
    show = gShow

instance eqColor :: Eq Color where
    eq = gEq

instance ordColor :: Ord Color where
    compare = gCompare

instance decodeJsonColor :: DecodeJson Color where
    decodeJson j = do
        str <- decodeJson j
        case str of
            "Red" -> pure Red
            "Green" -> pure Green
            "Blue" -> pure Blue
            "White" -> pure White
            "Black" -> pure Black
            _ -> Left "Invalid color"

instance encodeJsonColor :: EncodeJson Color where
    encodeJson c =
        case c of
            Red -> encodeJson "Red"
            Green -> encodeJson "Green"
            Blue -> encodeJson "Blue"
            White -> encodeJson "White"
            Black -> encodeJson "Black"

instance keyableColor :: Keyable Color where
    decodeKey s =
        case s of
            "Red" -> pure Red
            "Green" -> pure Green
            "Blue" -> pure Blue
            "White" -> pure White
            "Black" -> pure Black
            _ -> Left "Invalid color"
    encodeKey c =
        case c of
            Red -> "Red"
            Green -> "Green"
            Blue -> "Blue"
            White -> "White"
            Black -> "Black"

data ChipType
    = Basic Color
    | Gold

derive instance genericChipType :: Generic ChipType

instance showChipType :: Show ChipType where
    show = gShow

instance eqChipType :: Eq ChipType where
    eq = gEq

instance ordChipType :: Ord ChipType where
    compare = gCompare

instance keyableChipType :: Keyable ChipType where
    decodeKey s =
        case stripPrefix "Basic " s of
            Nothing ->
                if s == "Gold"
                then pure Gold
                else Left "Invalid chip type"
            Just colorStr ->
                case colorStr of
                    "Red" -> pure $ Basic Red
                    "Green" -> pure $ Basic Green
                    "Blue" -> pure $ Basic Blue
                    "White" -> pure $ Basic White
                    "Black" -> pure $ Basic Black
                    _ -> Left "Invalid basic chip color"
    encodeKey c =
        case c of
            Basic Red -> "Basic Red"
            Basic Green -> "Basic Green"
            Basic Blue -> "Basic Blue"
            Basic White -> "Basic White"
            Basic Black -> "Basic Black"
            Gold -> "Gold"

instance decodeJsonChipType :: DecodeJson ChipType where
    decodeJson j = do
        obj <- decodeJson j
        tag <- obj .? "tag"
        contents <- obj .? "contents"
        case tag of
            "Basic" -> do
                color <- decodeJson contents
                pure $ Basic color
            "Gold" -> pure Gold
            _ -> Left "Invalid ChipType tag"

instance encodeJsonChipType :: EncodeJson ChipType where
    encodeJson chip =
        case chip of
            Basic color ->
                "tag" := "Basic"
                ~> "contents" := encodeJson color
                ~> jsonEmptyObject
            Gold ->
                "tag" := "Gold"
                ~> "contents" := jsonEmptyArray
                ~> jsonEmptyObject

mapToJson :: forall a b. (Keyable a, EncodeJson b) => Map a b -> Json
mapToJson m =
    foldl (\obj (Tuple k v) -> encodeKey k := v ~> obj) jsonEmptyObject (Map.toList m)

mapFromJson :: forall a b. (Keyable a, Ord a, DecodeJson b) => Json -> Either String (Map a b)
mapFromJson json = do
    obj <- decodeJson json
    assocs <- traverse (\(Tuple k v) -> do
        k' <- decodeKey k
        pure (Tuple k' v)) (StrMap.toList obj)
    pure $ Map.fromList assocs

data Action
    = Take3 (Maybe Color) (Maybe Color) (Maybe Color)
    | Take2 Color
    | Reserve CardId
    | ReserveTop Int
    | Buy CardId
    | Discard (Map ChipType Int)
    | SelectNoble NobleId

instance decodeJsonAction :: DecodeJson Action where
    decodeJson j = do
        obj <- decodeJson j
        tag <- obj .? "tag"
        case tag of
            "Take3" -> do
                colorList <- obj .? "contents"
                case colorList of
                    [color1, color2, color3] -> pure $ Take3 color1 color2 color3
                    _ -> Left "Invalid contents for Take3 action"
            "Take2" -> do
                color <- obj .? "contents"
                pure $ Take2 color
            "Reserve" -> do
                cId <- obj .? "contents"
                pure $ Reserve cId
            "ReserveTop" -> do
                tier <- obj .? "contents"
                pure $ ReserveTop tier
            "Buy" -> do
                cId <- obj .? "contents"
                pure $ Buy cId
            "Discard" -> do
                cMapJson <- obj .? "contents"
                cMap <- mapFromJson cMapJson
                pure $ Discard cMap
            "SelectNoble" -> do
                nId <- obj .? "contents"
                pure $ SelectNoble nId
            _ -> Left "Invalid Action tag"

instance encodeJsonAction :: EncodeJson Action where
    encodeJson a =
        case a of
            Take3 color1 color2 color3 ->
                "tag" := "Take3"
                ~> "contents" := [color1, color2, color3]
                ~> jsonEmptyObject
            Take2 color ->
                "tag" := "Take2"
                ~> "contents" := color
                ~> jsonEmptyObject
            Reserve cId ->
                "tag" := "Reserve"
                ~> "contents" := cId
                ~> jsonEmptyObject
            ReserveTop tier ->
                "tag" := "ReserveTop"
                ~> "contents" := tier
                ~> jsonEmptyObject
            Buy cId ->
                "tag" := "Buy"
                ~> "contents" := cId
                ~> jsonEmptyObject
            Discard chips ->
                "tag" := "Discard"
                ~> "contents" := mapToJson chips
                ~> jsonEmptyObject
            SelectNoble nId ->
                "tag" := "SelectNoble"
                ~> "contents" := nId
                ~> jsonEmptyObject

data ActionSummary
    = Took3 (Array Color)
    | Took2 Color
    | Reserved Card
    | ReservedTop Int
    | Bought Card
    | Discarded (Map ChipType Int)
    | GainedNoble Noble

instance decodeJsonActionSummary :: DecodeJson ActionSummary where
    decodeJson j = do
        obj <- decodeJson j
        tag <- obj .? "tag"
        case tag of
            "Took3" -> do
                Took3 <$> obj .? "contents"
            "Took2" -> do
                Took2 <$> obj .? "contents"
            "Reserved" -> do
                Reserved <$> obj .? "contents"
            "ReservedTop" -> do
                ReservedTop <$> obj .? "contents"
            "Bought" -> do
                Bought <$> obj .? "contents"
            "Discarded" -> do
                cMapJson <- obj .? "contents"
                cMap <- mapFromJson cMapJson
                pure $ Discarded cMap
            "GainedNoble" -> do
                GainedNoble <$> obj .? "contents"
            _ -> Left "Invalid ActionSummary tag"

instance encodeJsonActionSummary :: EncodeJson ActionSummary where
    encodeJson as =
        case as of
            Took3 colors ->
                "tag" := "Took3"
                ~> "contents" := colors
                ~> jsonEmptyObject
            Took2 color ->
                "tag" := "Took2"
                ~> "contents" := color
                ~> jsonEmptyObject
            Reserved c ->
                "tag" := "Reserved"
                ~> "contents" := c
                ~> jsonEmptyObject
            ReservedTop t ->
                "tag" := "ReservedTop"
                ~> "contents" := t
                ~> jsonEmptyObject
            Bought c ->
                "tag" := "Bought"
                ~> "contents" := c
                ~> jsonEmptyObject
            Discarded chips ->
                "tag" := "Discarded"
                ~> "contents" := mapToJson chips
                ~> jsonEmptyObject
            GainedNoble n ->
                "tag" := "GainedNoble"
                ~> "contents" := n
                ~> jsonEmptyObject

newtype Card = Card
    { id :: CardId
    , color :: Color
    , points :: Int
    , cost :: Map Color Int
    }

instance decodeJsonCard :: DecodeJson Card where
    decodeJson json = do
        obj <- decodeJson json
        cId <- obj .? "_id_"
        cColor <- obj .? "_color"
        cCostJson <- obj .? "_cost"
        cCost <- mapFromJson cCostJson
        cPoints <- obj .? "_points"
        pure $ Card
            { id: cId
            , color: cColor
            , points: cPoints
            , cost: cCost
            }

instance encodeJsonCard :: EncodeJson Card where
    encodeJson (Card c) =
        "_id_" := c.id
        ~> "_color" := c.color
        ~> "_points" := c.points
        ~> "_cost" := mapToJson c.cost
        ~> jsonEmptyObject

newtype Noble = Noble
    { id :: NobleId
    , requirement :: Map Color Int
    , points :: Int
    }

instance decodeJsonNoble :: DecodeJson Noble where
    decodeJson json = do
        obj <- decodeJson json
        nId <- obj .? "_id_"
        nRequirementJson <- obj .? "_requirement"
        nRequirement <- mapFromJson nRequirementJson
        nPoints <- obj .? "_points"
        pure $ Noble
            { id: nId
            , requirement: nRequirement
            , points: nPoints
            }

instance encodeJsonNoble :: EncodeJson Noble where
    encodeJson (Noble n) =
        "_id_" := n.id
        ~> "_requirement" := mapToJson n.requirement
        ~> "_points" := n.points
        ~> jsonEmptyObject

newtype PlayerState = PlayerState
    { heldChips :: Map ChipType Int
    , ownedCards :: Array Card
    , ownedCardCounts :: Map Color Int
    , reservedCards :: Array Card
    , ownedNobles :: Array Noble
    , currentVP :: Int
    }

instance decodeJsonPlayerState :: DecodeJson PlayerState where
    decodeJson json = do
        obj <- decodeJson json
        heldChipsJson <- obj .? "_heldChips"
        heldChips <- mapFromJson heldChipsJson
        ownedCards <- obj .? "_ownedCards"
        ownedCardCountsJson <- obj .? "_ownedCardCounts"
        ownedCardCounts <- mapFromJson ownedCardCountsJson
        reservedCards <- obj .? "_reservedCards"
        ownedNobles <- obj .? "_ownedNobles"
        currentVP <- obj .? "_currentVP"
        pure $ PlayerState
            { heldChips: heldChips
            , ownedCards: ownedCards
            , ownedCardCounts: ownedCardCounts
            , reservedCards: reservedCards
            , ownedNobles: ownedNobles
            , currentVP: currentVP
            }

instance encodeJsonPlayerState :: EncodeJson PlayerState where
    encodeJson (PlayerState ps) =
        "_heldChips" := mapToJson ps.heldChips
        ~> "_ownedCards" := ps.ownedCards
        ~> "_ownedCardCounts" := mapToJson ps.ownedCardCounts
        ~> "_reservedCards" := ps.reservedCards
        ~> "_ownedNobles" := ps.ownedNobles
        ~> "_currentVP" := ps.currentVP
        ~> jsonEmptyObject

newtype PlayerView = PlayerView
    { heldChips :: Map ChipType Int
    , ownedCards :: Array Card
    , ownedCardCounts :: Map Color Int
    , reservedCardCount :: Int
    , ownedNobles :: Array Noble
    , currentVP :: Int
    }

instance decodeJsonPlayerView :: DecodeJson PlayerView where
    decodeJson json = do
        obj <- decodeJson json
        heldChipsJson <- obj .? "_heldChips"
        heldChips <- mapFromJson heldChipsJson
        ownedCards <- obj .? "_ownedCards"
        ownedCardCountsJson <- obj .? "_ownedCardCounts"
        ownedCardCounts <- mapFromJson ownedCardCountsJson
        reservedCardCount <- obj .? "_reservedCardCount"
        ownedNobles <- obj .? "_ownedNobles"
        currentVP <- obj .? "_currentVP"
        pure $ PlayerView
            { heldChips: heldChips
            , ownedCards: ownedCards
            , ownedCardCounts: ownedCardCounts
            , reservedCardCount: reservedCardCount
            , ownedNobles: ownedNobles
            , currentVP: currentVP
            }

instance encodeJsonPlayerView :: EncodeJson PlayerView where
    encodeJson (PlayerView pv) =
        "_heldChips" := mapToJson pv.heldChips
        ~> "_ownedCards" := pv.ownedCards
        ~> "_ownedCardCounts" := mapToJson pv.ownedCardCounts
        ~> "_reservedCardCount" := pv.reservedCardCount
        ~> "_ownedNobles" := pv.ownedNobles
        ~> "_currentVP" := pv.currentVP
        ~> jsonEmptyObject

newtype TierState = TierState
    { availableCards :: Array Card
    , tierDeck :: Array Card
    }

instance decodeJsonTierState :: DecodeJson TierState where
    decodeJson json = do
        obj <- decodeJson json
        availableCards <- obj .? "_availableCards"
        tierDeck <- obj .? "_tierDeck"
        pure $ TierState
            { availableCards: availableCards
            , tierDeck: tierDeck
            }

instance encodeJsonTierState :: EncodeJson TierState where
    encodeJson (TierState ts) =
        "_availableCards" := ts.availableCards
        ~> "_tierDeck" := ts.tierDeck
        ~> jsonEmptyObject

newtype TierView = TierView
    { availableCards :: Array Card
    , deckCount :: Int
    }

instance decodeJsonTierView :: DecodeJson TierView where
    decodeJson json = do
        obj <- decodeJson json
        availableCards <- obj .? "_availableCards"
        deckCount <- obj .? "_deckCount"
        pure $ TierView
            { availableCards: availableCards
            , deckCount: deckCount
            }

instance encodeJsonTierView :: EncodeJson TierView where
    encodeJson (TierView tv) =
        "_availableCards" := tv.availableCards
        ~> "_deckCount" := tv.deckCount
        ~> jsonEmptyObject

data ActionRequestType
    = TurnRequest
    | DiscardChipRequest Int
    | SelectNobleRequest

derive instance genericActionRequestType :: Generic ActionRequestType

instance showActionRequestType :: Show ActionRequestType where
    show = gShow

instance eqActionRequestType :: Eq ActionRequestType where
    eq = gEq

instance ordActionRequestType :: Ord ActionRequestType where
    compare = gCompare

instance decodeJsonActionRequestType :: DecodeJson ActionRequestType where
    decodeJson json = do
        obj <- decodeJson json
        tag <- obj .? "tag"
        case tag of
            "TurnRequest" -> pure TurnRequest
            "DiscardChipRequest" -> do
                n <- obj .? "contents"
                pure $ DiscardChipRequest n
            "SelectNobleRequest" -> pure SelectNobleRequest
            _ -> Left "Invalid ActionRequestType tag"

instance encodeJsonActionRequestType :: EncodeJson ActionRequestType where
    encodeJson req =
        case req of
            TurnRequest ->
                "tag" := "TurnRequest"
                ~> "contents" := jsonEmptyArray
                ~> jsonEmptyObject
            DiscardChipRequest n ->
                "tag" := "DiscardChipRequest"
                ~> "contents" := n
                ~> jsonEmptyObject
            SelectNobleRequest ->
                "tag" := "SelectNobleRequest"
                ~> "contents" := jsonEmptyArray
                ~> jsonEmptyObject

newtype ActionRequest = ActionRequest
    { player :: Int
    , type_ :: ActionRequestType
    }

instance decodeJsonActionRequest :: DecodeJson ActionRequest where
    decodeJson json = do
        obj <- decodeJson json
        player <- obj .? "_requestPlayer"
        type_ <- obj .? "_requestType"
        pure $ ActionRequest
            { player: player
            , type_: type_
            }

instance encodeJsonActionRequest :: EncodeJson ActionRequest where
    encodeJson (ActionRequest ar) =
        "_requestPlayer" := ar.player
        ~> "_requestType" := ar.type_
        ~> jsonEmptyObject

newtype GameState = GameState
    { numPlayers :: Int
    , playerStates :: Array PlayerState
    , availableChips :: Map ChipType Int
    , availableNobles :: Array Noble
    , tier1State :: TierState
    , tier2State :: TierState
    , tier3State :: TierState
    , currentRequest :: ActionRequest
    , actionLog :: Array (Tuple Int ActionSummary)
    }

instance decodeJsonGameState :: DecodeJson GameState where
    decodeJson json = do
        obj <- decodeJson json
        numPlayers <- obj .? "_numPlayers"
        playerStates <- obj .? "_playerStates"
        availableChipsJson <- obj .? "_availableChips"
        availableChips <- mapFromJson availableChipsJson
        availableNobles <- obj .? "_availableNobles"
        tier1State <- obj .? "_tier1State"
        tier2State <- obj .? "_tier2State"
        tier3State <- obj .? "_tier3State"
        currentRequest <- obj .? "_currentRequest"
        actionLog <- obj .? "_actionLog"
        pure $ GameState
            { numPlayers: numPlayers
            , playerStates: playerStates
            , availableChips: availableChips
            , availableNobles: availableNobles
            , tier1State: tier1State
            , tier2State: tier2State
            , tier3State: tier3State
            , currentRequest: currentRequest
            , actionLog: actionLog
            }

instance encodeJsonGameState :: EncodeJson GameState where
    encodeJson (GameState gs) =
        "_numPlayers" := gs.numPlayers
        ~> "_playerStates" := gs.playerStates
        ~> "_availableChips" := mapToJson gs.availableChips
        ~> "_availableNobles" := gs.availableNobles
        ~> "_tier1State" := gs.tier1State
        ~> "_tier2State" := gs.tier2State
        ~> "_tier3State" := gs.tier3State
        ~> "_currentRequest" := gs.currentRequest
        ~> "_actionLog" := gs.actionLog
        ~> jsonEmptyObject

newtype GameView = GameView
    { numPlayers :: Int
    , playerPosition :: Int
    , playerState :: PlayerState
    , opponentViews :: Array PlayerView
    , availableChips :: Map ChipType Int
    , availableNobles :: Array Noble
    , tier1View :: TierView
    , tier2View :: TierView
    , tier3View :: TierView
    , currentRequest :: ActionRequest
    , actionLog :: Array (Tuple Int ActionSummary)
    }

instance decodeJsonGameView :: DecodeJson GameView where
    decodeJson json = do
        obj <- decodeJson json
        numPlayers <- obj .? "_numPlayers"
        playerPosition <- obj .? "_playerPosition"
        playerState <- obj .? "_playerState"
        opponentViews <- obj .? "_opponentViews"
        availableChipsJson <- obj .? "_availableChips"
        availableChips <- mapFromJson availableChipsJson
        availableNobles <- obj .? "_availableNobles"
        tier1View <- obj .? "_tier1View"
        tier2View <- obj .? "_tier2View"
        tier3View <- obj .? "_tier3View"
        currentRequest <- obj .? "_currentRequest"
        actionLog <- obj .? "_actionLog"
        pure $ GameView
            { numPlayers: numPlayers
            , playerPosition: playerPosition
            , playerState: playerState
            , opponentViews: opponentViews
            , availableChips: availableChips
            , availableNobles: availableNobles
            , tier1View: tier1View
            , tier2View: tier2View
            , tier3View: tier3View
            , currentRequest: currentRequest
            , actionLog: actionLog
            }

instance encodeJsonGameView :: EncodeJson GameView where
    encodeJson (GameView gv) =
        "_numPlayers" := gv.numPlayers
        ~> "_playerPosition" := gv.playerPosition
        ~> "_playerState" := gv.playerState
        ~> "_opponentViews" := gv.opponentViews
        ~> "_availableChips" := mapToJson gv.availableChips
        ~> "_availableNobles" := gv.availableNobles
        ~> "_tier1View" := gv.tier1View
        ~> "_tier2View" := gv.tier2View
        ~> "_tier3View" := gv.tier3View
        ~> "_currentRequest" := gv.currentRequest
        ~> "_actionLog" := gv.actionLog
        ~> jsonEmptyObject

data GameResult
    = GameWinners (Array Int)

derive instance genericGameResult :: Generic GameResult

instance showGameResult :: Show GameResult where
    show = gShow

instance eqGameResult :: Eq GameResult where
    eq = gEq

instance ordGameResult :: Ord GameResult where
    compare = gCompare

instance decodeJsonGameResult :: DecodeJson GameResult where
    decodeJson json = do
        winners <- decodeJson json
        pure $ GameWinners winners

instance encodeJsonGameResult :: EncodeJson GameResult where
    encodeJson (GameWinners winners) =
        encodeJson winners

newtype PlayerInfo = PlayerInfo
    { displayName :: String
    }

derive instance genericPlayerInfo :: Generic PlayerInfo

instance decodeJsonPlayerInfo :: DecodeJson PlayerInfo where
    decodeJson json = do
        obj <- decodeJson json
        displayName <- obj .? "_displayName"
        pure $ PlayerInfo
            { displayName: displayName
            }

instance encodeJsonPlayerInfo :: EncodeJson PlayerInfo where
    encodeJson (PlayerInfo pi) =
        "_displayName" := pi.displayName
        ~> jsonEmptyObject

newtype RunningGame a = RunningGame
    { players :: Map Int PlayerInfo
    , version :: Int
    , gameState :: a
    }

instance decodeJsonRunningGame :: DecodeJson a => DecodeJson (RunningGame a) where
    decodeJson json = do
        obj <- decodeJson json
        playersJson <- obj .? "_players"
        players <- mapFromJson playersJson
        version <- obj .? "_version"
        gameState <- obj .? "_gameState"
        pure $ RunningGame
            { players: players
            , version: version
            , gameState: gameState
            }

instance encodeJsonRunningGame :: EncodeJson a => EncodeJson (RunningGame a) where
    encodeJson (RunningGame rg) =
        "_players" := mapToJson rg.players
        ~> "_version" := rg.version
        ~> "_gameState" := rg.gameState

data InstanceState
    = Waiting
    | Running
    | Completed

derive instance genericInstanceState :: Generic InstanceState

instance showInstanceState :: Show InstanceState where
    show = gShow

instance eqInstanceState :: Eq InstanceState where
    eq = gEq

instance decodeInstanceState :: DecodeJson InstanceState where
    decodeJson json = do
        str <- decodeJson json
        case str of
            "Waiting" -> pure Waiting
            "Running" -> pure Running
            "Completed" -> pure Completed
            _ -> Left "Invalid instance state"

instance encodeInstanceState :: EncodeJson InstanceState where
    encodeJson s =
        case s of
            Waiting -> encodeJson "Waiting"
            Running -> encodeJson "Running"
            Completed -> encodeJson "Completed"

newtype InstanceSummary = InstanceSummary
    { name :: String
    , players :: Array PlayerInfo
    , state :: InstanceState
    , maxPlayers :: Maybe Int
    }

derive instance genericInstanceSummary :: Generic InstanceSummary

instance showInstanceSummary :: Show InstanceSummary where
    show = gShow

instance decodeInstanceSummary :: DecodeJson InstanceSummary where
    decodeJson json = do
        obj <- decodeJson json
        name <- obj .? "_name"
        players <- obj .? "_players"
        state <- obj .? "_state"
        maxPlayers <- obj .? "_maxPlayers"
        pure $ InstanceSummary
            { name: name
            , players: players
            , state: state
            , maxPlayers: maxPlayers
            }

instance encodeInstanceSummary :: EncodeJson InstanceSummary where
    encodeJson (InstanceSummary is) =
        "_name" := is.name
        ~> "_players" := is.players
        ~> "_state" := is.state
        ~> "_maxPlayers" := is.maxPlayers
        ~> jsonEmptyObject

data InstanceViewDetails
    = WaitingInstanceView
        { waitingPlayers :: Array PlayerInfo
        , maxPlayers :: Int
        }
    | RunningInstanceView
        { runningGame :: RunningGame GameView
        }
    | CompletedInstanceView
        { completedGame :: RunningGame GameState
        , result :: GameResult
        }

instance decodeInstanceViewDetails :: DecodeJson InstanceViewDetails where
    decodeJson json = do
        obj <- decodeJson json
        tag <- obj .? "tag"
        case tag of
            "WaitingInstanceView" -> do
                waitingPlayers <- obj .? "_waitingPlayers"
                maxPlayers <- obj .? "_maxPlayers"
                pure $ WaitingInstanceView
                    { waitingPlayers: waitingPlayers
                    , maxPlayers: maxPlayers
                    }
            "RunningInstanceView" -> do
                runningGame <- obj .? "_runningGame"
                pure $ RunningInstanceView
                    { runningGame: runningGame
                    }
            "CompletedInstanceView" -> do
                completedGame <- obj .? "_completedGame"
                result <- obj .? "_result"
                pure $ CompletedInstanceView
                    { completedGame: completedGame
                    , result: result
                    }
            _ -> Left "Invalid InstanceView tag"

instance encodeInstanceViewDetails :: EncodeJson InstanceViewDetails where
    encodeJson ivd =
        case ivd of
            WaitingInstanceView wiv ->
                "tag" := "WaitingInstanceView"
                ~> "_waitingPlayers" := wiv.waitingPlayers
                ~> "_maxPlayers" := wiv.maxPlayers
                ~> jsonEmptyObject
            RunningInstanceView riv ->
                "tag" := "RunningInstanceView"
                ~> "_runningGame" := riv.runningGame
                ~> jsonEmptyObject
            CompletedInstanceView civ ->
                "tag" := "CompletedInstanceView"
                ~> "_completedGame" := civ.completedGame
                ~> "_result" := civ.result
                ~> jsonEmptyObject

newtype InstanceView
    = InstanceView
        { name :: String
        , details :: InstanceViewDetails
        }

instance decodeInstanceView :: DecodeJson InstanceView where
    decodeJson json = do
        obj <- decodeJson json
        name <- obj .? "_name"
        details <- obj .? "_details"
        pure $ InstanceView
            { name: name
            , details: details
            }

instance encodeInstanceView :: EncodeJson InstanceView where
    encodeJson (InstanceView iv) =
        "_name" := iv.name
        ~> "_details" := iv.details
        ~> jsonEmptyObject

newtype ServerRequest a = ServerRequest
    { playerKey :: String
    , requestData :: a
    }

instance decodeJsonServerRequest :: DecodeJson a => DecodeJson (ServerRequest a) where
    decodeJson json = do
        obj <- decodeJson json
        playerKey <- obj .? "_playerKey"
        requestData <- obj .? "_requestData"
        pure $ ServerRequest
            { playerKey: playerKey
            , requestData: requestData
            }

instance encodeJsonServerRequest :: EncodeJson a => EncodeJson (ServerRequest a) where
    encodeJson (ServerRequest sr) =
        "_playerKey" := sr.playerKey
        ~> "_requestData" := sr.requestData
        ~> jsonEmptyObject

newtype NewGameParams
    = NewGameParams
        { name :: String
        , maxPlayers :: Int
        }

derive instance genericNewGameParams :: Generic NewGameParams

instance decodeJsonNewGameParams :: DecodeJson NewGameParams where
    decodeJson json = do
        obj <- decodeJson json
        name <- obj .? "_name"
        maxPlayers <- obj .? "_maxPlayers"
        pure $ NewGameParams
            { name: name
            , maxPlayers: maxPlayers
            }

instance encodeJsonNewGameParams :: EncodeJson NewGameParams where
    encodeJson (NewGameParams params) =
        "_name" := params.name
        ~> "_maxPlayers" := params.maxPlayers
        ~> jsonEmptyObject

data RequestData
    = ListLobbies
    | NewLobby PlayerInfo NewGameParams
    | JoinLobby String PlayerInfo
    | LeaveLobby String
    | StartGame String
    | GetGameState String
    | GameAction String Action

instance decodeJsonRequestData :: DecodeJson RequestData where
    decodeJson json = do
        obj <- decodeJson json
        tag <- obj .? "tag"
        case tag of
            "ListLobbies" -> pure ListLobbies
            "NewLobby" -> do
                Tuple pInfo params <- obj .? "contents"
                pure $ NewLobby pInfo params
            "JoinLobby" -> do
                Tuple lobbyKey pInfo <- obj .? "contents"
                pure $ JoinLobby lobbyKey pInfo
            "LeaveLobby" -> do
                lobbyKey <- obj .? "contents"
                pure $ LeaveLobby lobbyKey
            "StartGame" -> do
                lobbyKey <- obj .? "contents"
                pure $ StartGame lobbyKey
            "GetGameState" -> do
                gameKey <- obj .? "contents"
                pure $ GetGameState gameKey
            "GameAction" -> do
                Tuple gameKey action <- obj .? "contents"
                pure $ GameAction gameKey action
            _ -> Left "Invalid RequestData tag"

instance encodeJsonRequestData :: EncodeJson RequestData where
    encodeJson dat =
        case dat of
            ListLobbies ->
                "tag" := "ListLobbies"
                ~> "contents" := jsonEmptyArray
                ~> jsonEmptyObject
            NewLobby pInfo params ->
                "tag" := "NewLobby"
                ~> "contents" := Tuple pInfo params
                ~> jsonEmptyObject
            JoinLobby lobbyKey pInfo ->
                "tag" := "JoinLobby"
                ~> "contents" := Tuple lobbyKey pInfo
                ~> jsonEmptyObject
            LeaveLobby lobbyKey ->
                "tag" := "LeaveLobby"
                ~> "contents" := lobbyKey
                ~> jsonEmptyObject
            StartGame lobbyKey ->
                "tag" := "StartGame"
                ~> "contents" := lobbyKey
                ~> jsonEmptyObject
            GetGameState gameKey ->
                "tag" := "GetGameState"
                ~> "contents" := gameKey
                ~> jsonEmptyObject
            GameAction gameKey action ->
                "tag" := "GameAction"
                ~> "contents" := Tuple gameKey action
                ~> jsonEmptyObject

data ServerResponse
    = ErrorResponse String
    | OkResponse Json

instance decodeJsonServerResponse :: DecodeJson ServerResponse where
    decodeJson json = do
        obj <- decodeJson json
        tag <- obj .? "tag"
        case tag of
            "ErrorResponse" -> do
                message <- obj .? "contents"
                pure $ ErrorResponse message
            "OkResponse" -> do
                contents <- obj .? "contents"
                pure $ OkResponse contents
            _ -> Left "Invalid ServerResponse tag"

instance encodeJsonServerResponse :: EncodeJson ServerResponse where
    encodeJson resp =
        case resp of
            ErrorResponse message ->
                "tag" := "ErrorResponse"
                ~> "contents" := message
                ~> jsonEmptyObject
            OkResponse contents ->
                "tag" := "OkResponse"
                ~> "contents" := contents
                ~> jsonEmptyObject
