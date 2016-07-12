{-# LANGUAGE MultiWayIf #-}
module Splendor.Rules where

import Control.Lens
import Control.Monad.State
import Data.Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Splendor.Types

illegal :: StateT s Maybe a
illegal = lift Nothing

withDefault :: a -> (a -> b) -> Maybe a -> Maybe b
withDefault def f = Just . f . fromMaybe def

runAction :: Action -> GameState -> Maybe (Maybe GameResult, GameState)
runAction a =
    runStateT $ do
        curTurn <- use (currentRequest . requestPlayer)
        curReqType <- use (currentRequest . requestType)
        case curReqType of
            TurnRequest -> do
                case a of
                    Take3 c1 c2 c3 -> do
                        chipSupply <- use availableChips
                        let colorSet = nub . catMaybes $ [c1, c2, c3]
                            availableColorSet =
                                filter
                                    (\c -> fromMaybe 0 (Map.lookup (Basic c) chipSupply) > 0)
                                    [Red, Green, Blue, White, Black]
                        if length colorSet < 3 && length colorSet /= length availableColorSet
                        then illegal
                        else do
                            for_ colorSet $ \c -> do
                                let colorSupply = fromMaybe 0 (Map.lookup (Basic c) chipSupply)
                                if colorSupply > 0
                                then do
                                    availableChips . at (Basic c) .= Just (colorSupply - 1)
                                    playerStates . ix curTurn . heldChips . at (Basic c) %= withDefault 0 (+1)
                                else illegal
                            resultingHeldChips <- preuse (playerStates . ix curTurn . heldChips)
                            case resultingHeldChips of
                                Nothing -> illegal
                                Just cmap -> do
                                    let numHeldChips = sum cmap 
                                    if numHeldChips > 10
                                    then do
                                        currentRequest .= ActionRequest
                                            { _requestPlayer = curTurn
                                            , _requestType = DiscardChipRequest (numHeldChips - 10)
                                            }
                                    else do
                                        nPlayers <- use numPlayers
                                        currentRequest .= ActionRequest
                                            { _requestPlayer = (curTurn + 1) `mod` nPlayers
                                            , _requestType = TurnRequest
                                            }
                    Take2 c -> do
                        chipSupply <- use availableChips
                        let colorSupply = fromMaybe 0 (Map.lookup (Basic c) chipSupply)
                        if colorSupply >= 4
                        then do
                            availableChips . at (Basic c) .= Just (colorSupply - 2)
                            playerStates . ix curTurn . heldChips . at (Basic c) %= withDefault 0 (+2)
                        else illegal
                        resultingHeldChips <- preuse (playerStates . ix curTurn . heldChips)
                        case resultingHeldChips of
                            Nothing -> illegal
                            Just cmap -> do
                                let numHeldChips = sum cmap 
                                if numHeldChips > 10
                                then do
                                    currentRequest .= ActionRequest
                                        { _requestPlayer = curTurn
                                        , _requestType = DiscardChipRequest (numHeldChips - 10)
                                        }
                                else do
                                    nPlayers <- use numPlayers
                                    currentRequest .= ActionRequest
                                        { _requestPlayer = (curTurn + 1) `mod` nPlayers
                                        , _requestType = TurnRequest
                                        }
                    Reserve cid -> do
                        alreadyReserved <- use (playerStates . ix curTurn . reservedCards)
                        when (length alreadyReserved >= 3) illegal
                        tier1Cards <- use (tier1State . availableCards)
                        tier2Cards <- use (tier2State . availableCards)
                        tier3Cards <- use (tier3State . availableCards)
                        goldAvailable <- fromMaybe 0 <$> use (availableChips . at Gold)
                        if | any (\c -> c^.cardId == cid) tier1Cards -> do
                                case filter (\c -> c^.cardId == cid) tier1Cards of
                                    [card] -> do
                                        playerStates . ix curTurn . reservedCards %= (card:)
                                        when (goldAvailable > 0) $ do
                                            playerStates . ix curTurn . heldChips . at Gold %= withDefault 0 (+1)
                                            availableChips . at Gold %= withDefault 0 (subtract 1)
                                        tier1State . availableCards %= filter (\c -> c^.cardId /= cid)
                                        deck <- use (tier1State . tierDeck)
                                        case deck of
                                            [] -> pure ()
                                            top:rest -> do
                                                tier1State . availableCards %= (top:)
                                                tier1State . tierDeck .= rest
                                    _ -> illegal
                           | any (\c -> c^.cardId == cid) tier2Cards -> do
                                case filter (\c -> c^.cardId == cid) tier2Cards of
                                    [card] -> do
                                        playerStates . ix curTurn . reservedCards %= (card:)
                                        when (goldAvailable > 0) $ do
                                            playerStates . ix curTurn . heldChips . at Gold %= withDefault 0 (+1)
                                            availableChips . at Gold %= withDefault 0 (subtract 1)
                                        tier2State . availableCards %= filter (\c -> c^.cardId /= cid)
                                        deck <- use (tier2State . tierDeck)
                                        case deck of
                                            [] -> pure ()
                                            top:rest -> do
                                                tier2State . availableCards %= (top:)
                                                tier2State . tierDeck .= rest
                                    _ -> illegal
                           | any (\c -> c^.cardId == cid) tier3Cards -> do
                                case filter (\c -> c^.cardId == cid) tier3Cards of
                                    [card] -> do
                                        playerStates . ix curTurn . reservedCards %= (card:)
                                        when (goldAvailable > 0) $ do
                                            playerStates . ix curTurn . heldChips . at Gold %= withDefault 0 (+1)
                                            availableChips . at Gold %= withDefault 0 (subtract 1)
                                        tier3State . availableCards %= filter (\c -> c^.cardId /= cid)
                                        deck <- use (tier3State . tierDeck)
                                        case deck of
                                            [] -> pure ()
                                            top:rest -> do
                                                tier3State . availableCards %= (top:)
                                                tier3State . tierDeck .= rest
                                    _ -> illegal
                           | otherwise -> illegal
                        resultingHeldChips <- preuse (playerStates . ix curTurn . heldChips)
                        case resultingHeldChips of
                            Nothing -> illegal
                            Just cmap -> do
                                let numHeldChips = sum cmap 
                                if numHeldChips > 10
                                then do
                                    currentRequest .= ActionRequest
                                        { _requestPlayer = curTurn
                                        , _requestType = DiscardChipRequest (numHeldChips - 10)
                                        }
                                else do
                                    nPlayers <- use numPlayers
                                    currentRequest .= ActionRequest
                                        { _requestPlayer = (curTurn + 1) `mod` nPlayers
                                        , _requestType = TurnRequest
                                        }
                    ReserveTop tier -> do
                        alreadyReserved <- use (playerStates . ix curTurn . reservedCards)
                        when (length alreadyReserved >= 3) illegal
                        availableGold <- fromMaybe 0 <$> use (availableChips . at Gold)
                        case tier of
                            1 -> do
                                deck <- use (tier1State . tierDeck)
                                case deck of
                                    [] -> illegal
                                    top:rest -> do
                                        playerStates . ix curTurn . reservedCards %= (top:)
                                        when (availableGold > 0) $ do
                                            playerStates . ix curTurn . heldChips . at Gold %= withDefault 0 (+1)
                                            availableChips . at Gold %= withDefault 0 (subtract 1)
                                        tier1State . tierDeck .= rest
                            2 -> do
                                deck <- use (tier2State . tierDeck)
                                case deck of
                                    [] -> illegal
                                    top:rest -> do
                                        playerStates . ix curTurn . reservedCards %= (top:)
                                        when (availableGold > 0) $ do
                                            playerStates . ix curTurn . heldChips . at Gold %= withDefault 0 (+1)
                                            availableChips . at Gold %= withDefault 0 (subtract 1)
                                        tier2State . tierDeck .= rest
                            3 -> do
                                deck <- use (tier3State . tierDeck)
                                case deck of
                                    [] -> illegal
                                    top:rest -> do
                                        playerStates . ix curTurn . reservedCards %= (top:)
                                        when (availableGold > 0) $ do
                                            playerStates . ix curTurn . heldChips . at Gold %= withDefault 0 (+1)
                                            availableChips . at Gold %= withDefault 0 (subtract 1)
                                        tier3State . tierDeck .= rest
                            _ -> illegal
                        resultingHeldChips <- preuse (playerStates . ix curTurn . heldChips)
                        case resultingHeldChips of
                            Nothing -> illegal
                            Just cmap -> do
                                let numHeldChips = sum cmap 
                                if numHeldChips > 10
                                then do
                                    currentRequest .= ActionRequest
                                        { _requestPlayer = curTurn
                                        , _requestType = DiscardChipRequest (numHeldChips - 10)
                                        }
                                else do
                                    nPlayers <- use numPlayers
                                    currentRequest .= ActionRequest
                                        { _requestPlayer = (curTurn + 1) `mod` nPlayers
                                        , _requestType = TurnRequest
                                        }
                    Buy cid -> do
                        tier1Cards <- use (tier1State . availableCards)
                        tier2Cards <- use (tier2State . availableCards)
                        tier3Cards <- use (tier3State . availableCards)
                        resCards <- use (playerStates . ix curTurn . reservedCards)
                        if | any (\c -> c^.cardId == cid) tier1Cards -> do
                                case filter (\c -> c^.cardId == cid) tier1Cards of
                                    [card] -> do
                                        playerStates . ix curTurn . ownedCards %= (card:)
                                        playerStates . ix curTurn . ownedCardCounts . at (card^.cardColor) %= withDefault 0 (+1)
                                        playerStates . ix curTurn . currentVP += card^.cardPoints
                                        tier1State . availableCards %= filter (\c -> c^.cardId /= cid)
                                        for_ (Map.toList (card^.cardCost)) $ \(color, n) -> do
                                            numBasics <- fromMaybe 0 <$> preuse (playerStates . ix curTurn . heldChips . ix (Basic color))
                                            numCards <- fromMaybe 0 <$> preuse (playerStates . ix curTurn . ownedCardCounts . ix color)
                                            let colorPayment = max 0 (n - numCards)
                                            if colorPayment <= numBasics
                                            then do
                                                playerStates . ix curTurn . heldChips . at (Basic color) %= withDefault 0 (subtract colorPayment)
                                                availableChips . at (Basic color) %= withDefault 0 (+ colorPayment)
                                            else do
                                                playerStates . ix curTurn . heldChips . at (Basic color) .= Nothing
                                                availableChips . at (Basic color) %= withDefault 0 (+ numBasics)
                                                playerStates . ix curTurn . heldChips . at Gold %= withDefault 0 (subtract (colorPayment - numBasics))
                                                availableChips . at Gold %= withDefault 0 (+ (colorPayment - numBasics))
                                        resultingChipCounts <- use (playerStates . ix curTurn . heldChips)
                                        when (any (< 0) resultingChipCounts) illegal
                                        deck <- use (tier1State . tierDeck)
                                        case deck of
                                            [] -> pure ()
                                            top:rest -> do
                                                tier1State . availableCards %= (top:)
                                                tier1State . tierDeck .= rest
                                    _ -> illegal
                           | any (\c -> c^.cardId == cid) tier2Cards -> do
                                case filter (\c -> c^.cardId == cid) tier2Cards of
                                    [card] -> do
                                        playerStates . ix curTurn . ownedCards %= (card:)
                                        playerStates . ix curTurn . ownedCardCounts . at (card^.cardColor) %= withDefault 0 (+1)
                                        playerStates . ix curTurn . currentVP += card^.cardPoints
                                        tier2State . availableCards %= filter (\c -> c^.cardId /= cid)
                                        for_ (Map.toList (card^.cardCost)) $ \(color, n) -> do
                                            numBasics <- fromMaybe 0 <$> preuse (playerStates . ix curTurn . heldChips . ix (Basic color))
                                            numCards <- fromMaybe 0 <$> preuse (playerStates . ix curTurn . ownedCardCounts . ix color)
                                            let colorPayment = max 0 (n - numCards)
                                            if colorPayment <= numBasics
                                            then do
                                                playerStates . ix curTurn . heldChips . at (Basic color) %= withDefault 0 (subtract colorPayment)
                                                availableChips . at (Basic color) %= withDefault 0 (+ colorPayment)
                                            else do
                                                playerStates . ix curTurn . heldChips . at (Basic color) .= Nothing
                                                availableChips . at (Basic color) %= withDefault 0 (+ numBasics)
                                                playerStates . ix curTurn . heldChips . at Gold %= withDefault 0 (subtract (colorPayment - numBasics))
                                                availableChips . at Gold %= withDefault 0 (+ (colorPayment - numBasics))
                                        resultingChipCounts <- use (playerStates . ix curTurn . heldChips)
                                        when (any (< 0) resultingChipCounts) illegal
                                        deck <- use (tier2State . tierDeck)
                                        case deck of
                                            [] -> pure ()
                                            top:rest -> do
                                                tier2State . availableCards %= (top:)
                                                tier2State . tierDeck .= rest
                                    _ -> illegal
                           | any (\c -> c^.cardId == cid) tier3Cards -> do
                                case filter (\c -> c^.cardId == cid) tier3Cards of
                                    [card] -> do
                                        playerStates . ix curTurn . ownedCards %= (card:)
                                        playerStates . ix curTurn . ownedCardCounts . at (card^.cardColor) %= withDefault 0 (+1)
                                        playerStates . ix curTurn . currentVP += card^.cardPoints
                                        tier3State . availableCards %= filter (\c -> c^.cardId /= cid)
                                        for_ (Map.toList (card^.cardCost)) $ \(color, n) -> do
                                            numBasics <- fromMaybe 0 <$> preuse (playerStates . ix curTurn . heldChips . ix (Basic color))
                                            numCards <- fromMaybe 0 <$> preuse (playerStates . ix curTurn . ownedCardCounts . ix color)
                                            let colorPayment = max 0 (n - numCards)
                                            if colorPayment <= numBasics
                                            then do
                                                playerStates . ix curTurn . heldChips . at (Basic color) %= withDefault 0 (subtract colorPayment)
                                                availableChips . at (Basic color) %= withDefault 0 (+ colorPayment)
                                            else do
                                                playerStates . ix curTurn . heldChips . at (Basic color) .= Nothing
                                                availableChips . at (Basic color) %= withDefault 0 (+ numBasics)
                                                playerStates . ix curTurn . heldChips . at Gold %= withDefault 0 (subtract (colorPayment - numBasics))
                                                availableChips . at Gold %= withDefault 0 (+ (colorPayment - numBasics))
                                        resultingChipCounts <- use (playerStates . ix curTurn . heldChips)
                                        when (any (< 0) resultingChipCounts) illegal
                                        deck <- use (tier3State . tierDeck)
                                        case deck of
                                            [] -> pure ()
                                            top:rest -> do
                                                tier3State . availableCards %= (top:)
                                                tier3State . tierDeck .= rest
                                    _ -> illegal
                           | any (\c -> c^.cardId == cid) resCards -> do
                                case filter (\c -> c^.cardId == cid) resCards of
                                    [card] -> do
                                        playerStates . ix curTurn . ownedCards %= (card:)
                                        playerStates . ix curTurn . ownedCardCounts . at (card^.cardColor) %= withDefault 0 (+1)
                                        playerStates . ix curTurn . currentVP += card^.cardPoints
                                        playerStates . ix curTurn . reservedCards %= filter (\c -> c^.cardId /= cid)
                                        for_ (Map.toList (card^.cardCost)) $ \(color, n) -> do
                                            numBasics <- fromMaybe 0 <$> preuse (playerStates . ix curTurn . heldChips . ix (Basic color))
                                            numCards <- fromMaybe 0 <$> preuse (playerStates . ix curTurn . ownedCardCounts . ix color)
                                            let colorPayment = max 0 (n - numCards)
                                            if colorPayment <= numBasics
                                            then do
                                                playerStates . ix curTurn . heldChips . at (Basic color) %= withDefault 0 (subtract colorPayment)
                                                availableChips . at (Basic color) %= withDefault 0 (+ colorPayment)
                                            else do
                                                playerStates . ix curTurn . heldChips . at (Basic color) .= Nothing
                                                availableChips . at (Basic color) %= withDefault 0 (+ numBasics)
                                                playerStates . ix curTurn . heldChips . at Gold %= withDefault 0 (subtract (colorPayment - numBasics))
                                                availableChips . at Gold %= withDefault 0 (+ (colorPayment - numBasics))
                                        resultingChipCounts <- use (playerStates . ix curTurn . heldChips)
                                        when (any (< 0) resultingChipCounts) illegal
                                    _ -> illegal
                           | otherwise -> illegal
                        -- Check for matching nobles.
                        boardNobles <- use availableNobles
                        playerCards <- use (playerStates . ix curTurn . ownedCardCounts)
                        let matchingNobles =
                                filter
                                    (\noble -> all
                                        (\(color, k) -> fromMaybe 0 (Map.lookup color playerCards) >= k)
                                        (Map.toList (noble^.nobleRequirement)))
                                    boardNobles
                        case matchingNobles of
                            [] -> do
                                nPlayers <- use numPlayers
                                currentRequest .= ActionRequest
                                    { _requestPlayer = (curTurn + 1) `mod` nPlayers
                                    , _requestType = TurnRequest
                                    }
                            [noble] -> do
                                availableNobles %= filter (/= noble)
                                playerStates . ix curTurn . ownedNobles %= (noble:)
                                playerStates . ix curTurn . currentVP += (noble^.noblePoints)
                                nPlayers <- use numPlayers
                                currentRequest .= ActionRequest
                                    { _requestPlayer = (curTurn + 1) `mod` nPlayers
                                    , _requestType = TurnRequest
                                    }
                            _ -> do
                                currentRequest .= ActionRequest
                                    { _requestPlayer = curTurn
                                    , _requestType = SelectNobleRequest
                                    }
                    _ -> illegal
            DiscardChipRequest n -> do
                case a of
                    Discard cmap -> do
                        when (sum cmap /= n) illegal
                        for_ (Map.toList cmap) $ \(cType, k) -> do
                            playerStates . ix curTurn . heldChips . at cType %= withDefault 0 (subtract k)
                            availableChips . at cType %= withDefault 0 (+k)
                        resultingChips <- use (playerStates . ix curTurn . heldChips)
                        when (any (< 0) resultingChips) illegal
                        nPlayers <- use numPlayers
                        currentRequest .= ActionRequest
                            { _requestPlayer = (curTurn + 1) `mod` nPlayers
                            , _requestType = TurnRequest
                            }
                    _ -> illegal
            SelectNobleRequest -> do
                case a of
                    SelectNoble nid -> do
                        boardNobles <- use availableNobles
                        let matchingNobles = filter (\n -> n^.nobleId == nid) boardNobles
                        case matchingNobles of
                            [noble] -> do
                                availableNobles %= filter (/= noble)
                                playerStates . ix curTurn . ownedNobles %= (noble:)
                                playerStates . ix curTurn . currentVP += (noble^.noblePoints)
                                nPlayers <- use numPlayers
                                currentRequest .= ActionRequest
                                    { _requestPlayer = (curTurn + 1) `mod` nPlayers
                                    , _requestType = TurnRequest
                                    }
                            _ -> illegal
                    _ -> illegal
        newReq <- use currentRequest
        if newReq == ActionRequest { _requestPlayer = 0, _requestType = TurnRequest }
        then
            checkEndOfGame
        else
            pure Nothing

checkEndOfGame :: StateT GameState Maybe (Maybe GameResult)
checkEndOfGame = do
    pStates <- use playerStates
    if any (\pState -> pState^.currentVP >= 15) pStates
    then do
        let stateValue pState = (pState^.currentVP, -length (pState^.ownedCards))
            winningValue = maximum . fmap stateValue $ pStates
            winners = map fst . filter ((== winningValue) . stateValue . snd) . zip [0..] . toList $ pStates
        pure (Just (GameWinners winners))
    else
        pure Nothing
