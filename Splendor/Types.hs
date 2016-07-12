{-# LANGUAGE TemplateHaskell #-}
module Splendor.Types where

import Control.Lens
import Data.Map (Map)
import Data.Vector (Vector)

newtype CardId = CardId Int
    deriving (Eq, Show, Ord)

newtype NobleId = NobleId Int
    deriving (Eq, Show, Ord)

data Color
    = Red
    | Green
    | Blue
    | White
    | Black
    deriving (Eq, Show, Ord, Enum, Bounded)

data ChipType
    = Basic Color
    | Gold
    deriving (Eq, Show, Ord)

data Action
    = Take3 (Maybe Color) (Maybe Color) (Maybe Color)
    | Take2 Color
    | Reserve CardId
    | ReserveTop Int
    | Buy CardId
    | Discard (Map ChipType Int)
    | SelectNoble NobleId
    deriving (Eq, Show, Ord)

data Card
    = Card
        { _cardId :: CardId
        , _cardColor :: Color
        , _cardPoints :: Int
        , _cardCost :: Map Color Int
        }
    deriving (Eq, Show, Ord)

data Noble
    = Noble
        { _nobleId :: NobleId
        , _nobleRequirement :: Map Color Int
        , _noblePoints :: Int
        }
    deriving (Eq, Show, Ord)

data PlayerState
    = PlayerState
        { _heldChips :: Map ChipType Int
        , _ownedCards :: [Card]
        , _ownedCardCounts :: Map Color Int
        , _reservedCards :: [Card]
        , _ownedNobles :: [Noble]
        , _currentVP :: Int
        }
    deriving (Eq, Show, Ord)

data TierState
    = TierState
        { _availableCards :: [Card]
        , _tierDeck :: [Card]
        }
    deriving (Eq, Show, Ord)

data ActionRequestType
    = TurnRequest
    | DiscardChipRequest Int
    | SelectNobleRequest
    deriving (Eq, Show, Ord)

data ActionRequest
    = ActionRequest
        { _requestPlayer :: Int
        , _requestType :: ActionRequestType
        }
    deriving (Eq, Show, Ord)

data GameState
    = GameState
        { _numPlayers :: Int
        , _playerStates :: Vector PlayerState
        , _availableChips :: Map ChipType Int
        , _availableNobles :: [Noble]
        , _tier1State :: TierState
        , _tier2State :: TierState
        , _tier3State :: TierState
        , _currentRequest :: ActionRequest
        }
    deriving (Eq, Show, Ord)

data GameResult
    = GameWinners [Int]
    deriving (Eq, Show, Ord)

makeLenses ''Card
makeLenses ''Noble
makeLenses ''ActionRequest
makeLenses ''PlayerState
makeLenses ''TierState
makeLenses ''GameState
