module Splendor.Types where

import Prelude

import Data.Argonaut
import Data.Either
import Data.Generic
import Data.Maybe
import Data.Map
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
data Action
    = Take3 (Maybe Color) (Maybe Color)
    | Take2 Color
    | Reserve CardId
    | ReserveTop Int
    | Buy CardId
    | Discard (Array (Tuple ChipType Int))
    | SelectNoble NobleId

derive instance genericAction :: Generic Action

instance showAction :: Show Action where
    show = gShow

instance eqAction :: Eq Action where
    eq = gEq

instance ordAction :: Ord Action where
    compare = gCompare

type Card =
    { id :: CardId
    , color :: Color
    , points :: Int
    , cost :: Map Color Int
    }

type Noble =
    { id :: NobleId
    , requirement :: Map Color Int
    , points :: Int
    }

type PlayerState =
    { heldChips :: Map ChipType Int
    , ownedCards :: Array Card
    , ownedCardCounts :: Map Color Int
    , reservedCards :: Array Card
    , ownedNobles :: Array Noble
    , currentVP :: Int
    }

type PlayerView =
    { heldChips :: Map ChipType Int
    , ownedCards :: Array Card
    , ownedCardCounts :: Map Color Int
    , reservedCardCount :: Int
    , ownedNobles :: Array Noble
    , currentVP :: Int
    }

type TierState =
    { availableCards :: Array Card
    , tierDeck :: Array Card
    }

type TierView =
    { availableCards :: Array Card
    , deckCount :: Int
    }

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

type ActionRequest =
    { player :: Int
    , type_ :: ActionRequestType
    }

type GameState =
    { numPlayers :: Int
    , playerStates :: Array PlayerState
    , availableChips :: Map ChipType Int
    , availableNobles :: Array Noble
    , tier1State :: TierState
    , tier2State :: TierState
    , tier3State :: TierState
    , currentRequest :: ActionRequest
    }

type GameView =
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
    }

data GameResult
    = GameWinners (Array Int)

derive instance genericGameResult :: Generic GameResult

instance showGameResult :: Show GameResult where
    show = gShow

instance eqGameResult :: Eq GameResult where
    eq = gEq

instance ordGameResult :: Ord GameResult where
    compare = gCompare

type PlayerInfo =
    { displayName :: String
    }

type RunningGame a =
    { players :: Map Int PlayerInfo
    , version :: Int
    , gameState :: a
    }

type LobbyView =
    { waitingPlayers :: Array PlayerInfo
    }

type ServerRequest a =
    { playerKey :: String
    , requestData :: a
    }

data RequestData
    = ListLobbies
    | NewLobby PlayerInfo
    | JoinLobby String PlayerInfo
    | StartGame String
    | GetGameState String
    | GameAction String Action
