module Splendor.Data
    ( tier1CardSet
    , tier2CardSet
    , tier3CardSet
    , nobleSet
    ) where

import qualified Data.Map as Map
import Splendor.Types

card :: Color -> Int -> [(Color, Int)] -> Card
card c p cost =
    Card
        { _cardId = CardId 0
        , _cardColor = c
        , _cardPoints = p
        , _cardCost = Map.fromList cost
        }

tier1CardSet :: [Card]
tier1CardSet =
    [ card Black 0 [(White, 1), (Blue, 1), (Green, 1), (Red, 1)]
    , card Black 0 [(White, 1), (Blue, 2), (Green, 1), (Red, 1)]
    , card Black 0 [(White, 2), (Blue, 2), (Red, 1)]
    , card Black 0 [(Green, 1), (Red, 3), (Black, 1)]
    , card Black 0 [(Green, 2), (Red, 1)]
    , card Black 0 [(White, 2), (Green, 2)]
    , card Black 0 [(Green, 3)]
    , card Black 1 [(Blue, 4)]
    , card Blue 0 [(White, 1), (Green, 1), (Red, 1), (Black, 1)]
    , card Blue 0 [(White, 1), (Green, 1), (Red, 2), (Black, 1)]
    , card Blue 0 [(White, 1), (Green, 2), (Red, 2)]
    , card Blue 0 [(Blue, 1), (Green, 3), (Red, 1)]
    , card Blue 0 [(White, 1), (Black, 2)]
    , card Blue 0 [(Green, 2), (Black, 2)]
    , card Blue 0 [(Black, 3)]
    , card Blue 1 [(Red, 4)]
    , card White 0 [(Blue, 1), (Green, 1), (Red, 1), (Black, 1)]
    , card White 0 [(Blue, 1), (Green, 2), (Red, 1), (Black, 1)]
    , card White 0 [(Blue, 2), (Green, 2), (Black, 1)]
    , card White 0 [(White, 3), (Blue, 1), (Black, 1)]
    , card White 0 [(Red, 2), (Black, 1)]
    , card White 0 [(Blue, 2), (Black, 2)]
    , card White 0 [(Blue, 3)]
    , card White 1 [(Green, 4)]
    , card Green 0 [(White, 1), (Blue, 1), (Red, 1), (Black, 1)]
    , card Green 0 [(White, 1),  (Blue, 1), (Red, 1), (Black, 2)]
    , card Green 0 [(Blue, 1), (Red, 2), (Black, 2)]
    , card Green 0 [(White, 1), (Blue, 3), (Green, 1)]
    , card Green 0 [(White, 2), (Blue, 1)]
    , card Green 0 [(Blue, 2), (Red, 2)]
    , card Green 0 [(Red, 3)]
    , card Green 1 [(Black, 4)]
    , card Red 0 [(White, 1), (Blue, 1), (Green, 1), (Black, 1)]
    , card Red 0 [(White, 2),  (Blue, 1), (Green, 1), (Black, 1)]
    , card Red 0 [(White, 2), (Green, 1), (Black, 2)]
    , card Red 0 [(White, 1), (Red, 1), (Black, 3)]
    , card Red 0 [(Blue, 2), (Green, 1)]
    , card Red 0 [(White, 2), (Red, 2)]
    , card Red 0 [(White, 3)]
    , card Red 1 [(White, 4)]
    ]

tier2CardSet :: [Card]
tier2CardSet =
    [ card Black 1 [(White, 3), (Blue, 2), (Green, 2)]
    , card Black 1 [(White, 3), (Green, 3), (Black, 2)]
    , card Black 2 [(Blue, 1), (Green, 4), (Red, 2)]
    , card Black 2 [(Green, 5), (Red, 3)]
    , card Black 2 [(White, 5)]
    , card Black 3 [(Black, 6)]
    , card Blue 1 [(Blue, 2), (Green, 2), (Red, 3)]
    , card Blue 1 [(Blue, 2), (Green, 3), (Black, 3)]
    , card Blue 2 [(White, 2), (Red, 1), (Black, 4)]
    , card Blue 2 [(White, 5), (Blue, 3)]
    , card Blue 2 [(Blue, 5)]
    , card Blue 3 [(Blue, 6)]
    , card White 1 [(Green, 3), (Red, 2), (Black, 2)]
    , card White 1 [(White, 2), (Blue, 3), (Red, 3)]
    , card White 2 [(Green, 1), (Red, 4), (Black, 2)]
    , card White 2 [(Red, 5), (Black, 3)]
    , card White 2 [(Red, 5)]
    , card White 3 [(White, 6)]
    , card Green 1 [(White, 2), (Blue, 3), (Black, 2)]
    , card Green 1 [(White, 3), (Green, 2), (Red, 3)]
    , card Green 2 [(White, 4), (Blue, 2), (Black, 1)]
    , card Green 2 [(Blue, 5), (Green, 3)]
    , card Green 2 [(Green, 5)]
    , card Green 3 [(Green, 6)]
    , card Red 1 [(White, 2), (Red, 2), (Black, 3)]
    , card Red 1 [(Blue, 3), (Red, 2), (Black, 3)]
    , card Red 2 [(White, 1), (Blue, 4), (Green, 2)]
    , card Red 2 [(Green, 3), (Black, 5)]
    , card Red 2 [(Black, 5)]
    , card Red 3 [(Red, 6)]
    ]

tier3CardSet :: [Card]
tier3CardSet =
    [ card Black 3 [(White, 3), (Blue, 3), (Green, 5), (Red, 3)]
    , card Black 4 [(Red, 7)]
    , card Black 4 [(Green, 3), (Red, 6), (Black, 3)]
    , card Black 5 [(Red, 7), (Black, 3)]
    , card Blue 3 [(White, 3), (Green, 3), (Red, 3), (Black, 5)]
    , card Blue 4 [(White, 7)]
    , card Blue 4 [(White, 6), (Blue, 3), (Black, 3)]
    , card Blue 5 [(White, 7), (Blue, 3)]
    , card White 3 [(Blue, 3), (Green, 3), (Red, 5), (Black, 3)]
    , card White 4 [(Black, 7)]
    , card White 4 [(White, 3), (Red, 3), (Black, 6)]
    , card White 5 [(White, 3), (Black, 7)]
    , card Green 3 [(White, 5), (Blue, 3), (Red, 3), (Black, 3)]
    , card Green 4 [(Blue, 7)]
    , card Green 4 [(White, 3), (Blue, 6), (Green, 3)]
    , card Green 5 [(Blue, 7), (Green, 3)]
    , card Red 3 [(White, 3), (Blue, 5), (Green, 3), (Black, 3)]
    , card Red 4 [(Green, 7)]
    , card Red 4 [(Blue, 3), (Green, 6), (Red, 3)]
    , card Red 5 [(Green, 7), (Red, 3)]
    ]

noble :: Int -> [(Color, Int)] -> Noble
noble p req =
    Noble
        { _nobleId = NobleId 0
        , _nobleRequirement = Map.fromList req
        , _noblePoints = p
        }

nobleSet :: [Noble]
nobleSet =
    [ noble 3 [(Red, 4), (Green, 4)]
    , noble 3 [(Blue, 4), (Green, 4)]
    , noble 3 [(Blue, 4), (White, 4)]
    , noble 3 [(Black, 4), (White, 4)]
    , noble 3 [(Black, 4), (Red, 4)]
    , noble 3 [(Black, 3), (Red, 3), (White, 3)]
    , noble 3 [(Green, 3), (Blue, 3), (Red, 3)]
    , noble 3 [(Green, 3), (Blue, 3), (White, 3)]
    , noble 3 [(Black, 3), (Blue, 3), (White, 3)]
    , noble 3 [(Black, 3), (Red, 3), (Green, 3)]
    ]
