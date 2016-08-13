module Splendor.Rules where

import Prelude

import Data.Array as Array
import Data.List as List
import Data.Foldable
import Data.Map as Map
import Data.Maybe
import Data.Tuple

import Splendor.Types

isLegalAction :: GameView -> Action -> Boolean
isLegalAction (GameView gv) act =
    case gv.currentRequest of
        ActionRequest ar ->
            if ar.player /= gv.playerPosition
            then false
            else case ar.type_ of
                TurnRequest -> case act of
                    Take3 c1 c2 c3 ->
                        let
                        colors = Array.nub $ Array.catMaybes [c1, c2, c3]
                        numAvailableColors = List.length <<< List.filter (\(Tuple ctype n) -> ctype /= Gold && n > 0) $ Map.toList gv.availableChips
                        in
                        Array.length colors == 3 || Array.length colors == numAvailableColors
                    Take2 c ->
                        fromMaybe 0 (Map.lookup (Basic c) gv.availableChips) >= 4
                    Reserve cid ->
                        case gv.playerState of
                            PlayerState ps ->
                                any (\(TierView tv) ->
                                    any (\(Card c) -> c.id == cid) tv.availableCards
                                ) [gv.tier1View, gv.tier2View, gv.tier3View]
                                && Array.length ps.reservedCards < 3
                    ReserveTop tier ->
                        case gv.playerState of
                            PlayerState ps ->
                                Array.length ps.reservedCards < 3
                                && (\(TierView tv) -> tv.deckCount > 0) (case tier of
                                    1 -> gv.tier1View
                                    2 -> gv.tier2View
                                    3 -> gv.tier3View
                                    _ -> TierView
                                        { availableCards: []
                                        , deckCount: 0
                                        }
                                )
                    Buy cid ->
                        case gv.playerState of
                            PlayerState ps ->
                                let
                                candidateCards =
                                    foldMap (\(TierView tv) -> tv.availableCards) [gv.tier1View, gv.tier2View, gv.tier3View]
                                    <> ps.reservedCards
                                matchingCards = Array.filter (\(Card c) ->c.id == cid) candidateCards
                                in
                                Array.length matchingCards == 1
                                && case Array.index matchingCards 0 of
                                    Nothing -> false
                                    Just (Card c) ->
                                        let
                                        requiredGold = sum $ map
                                            (\color ->
                                                let
                                                numBasics = fromMaybe 0 (Map.lookup (Basic color) ps.heldChips)
                                                colorCost = fromMaybe 0 (Map.lookup color c.cost)
                                                in
                                                max 0 (colorCost - numBasics)
                                            ) [Red, Green, Blue, White, Black]
                                        in
                                        fromMaybe 0 (Map.lookup Gold ps.heldChips) >= requiredGold
                    _ -> false
                DiscardChipRequest n ->
                    case act of
                        Discard discards ->
                            case gv.playerState of
                                PlayerState ps ->
                                    sum discards == n
                                    && all (\(Tuple ctype k) ->
                                        k <= fromMaybe 0 (Map.lookup ctype ps.heldChips)
                                    ) (Map.toList discards)
                        _ -> false
                SelectNobleRequest ->
                    case act of
                        SelectNoble nid ->
                            any (\(Noble n) -> n.id == nid) gv.availableNobles
                        _ -> false
