{-# LANGUAGE MultiWayIf #-}
module Splendor.Rules where

import Control.Lens
import Control.Monad.State
import Control.Monad.Random.Class
import Data.Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Vector as Vector
import System.Random.Shuffle

import Splendor.Types
import Splendor.Data

illegal :: StateT s Maybe a
illegal = lift Nothing

withDefault :: a -> (a -> b) -> Maybe a -> Maybe b
withDefault def f = Just . f . fromMaybe def

runAction :: Int -> Action -> GameState -> Maybe (Maybe GameResult, GameState)
runAction idx a =
    runStateT $ do
        curTurn <- use (currentRequest . requestPlayer)
        when (idx /= curTurn) illegal
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
                                        playerStates . ix curTurn . ownedCards %= (card:)
                                        playerStates . ix curTurn . ownedCardCounts . at (card^.cardColor) %= withDefault 0 (+1)
                                        playerStates . ix curTurn . currentVP += card^.cardPoints
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
                                        playerStates . ix curTurn . ownedCards %= (card:)
                                        playerStates . ix curTurn . ownedCardCounts . at (card^.cardColor) %= withDefault 0 (+1)
                                        playerStates . ix curTurn . currentVP += card^.cardPoints
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
                                        playerStates . ix curTurn . ownedCards %= (card:)
                                        playerStates . ix curTurn . ownedCardCounts . at (card^.cardColor) %= withDefault 0 (+1)
                                        playerStates . ix curTurn . currentVP += card^.cardPoints
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
                                        playerStates . ix curTurn . ownedCards %= (card:)
                                        playerStates . ix curTurn . ownedCardCounts . at (card^.cardColor) %= withDefault 0 (+1)
                                        playerStates . ix curTurn . currentVP += card^.cardPoints
                                        playerStates . ix curTurn . reservedCards %= filter (\c -> c^.cardId /= cid)
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

initState :: MonadRandom m => Int -> m (Maybe GameState)
initState n = do
    tier1CardOrder <- shuffleM tier1CardSet
    tier2CardOrder <- shuffleM tier2CardSet
    tier3CardOrder <- shuffleM tier3CardSet
    let numCards1 = length tier1CardOrder
        numCards2 = length tier2CardOrder
        numCards3 = length tier3CardOrder
        tier1Deck = zipWith (\card i -> card & cardId .~ CardId i) tier1CardOrder [1 .. numCards1]
        tier2Deck = zipWith (\card i -> card & cardId .~ CardId i) tier2CardOrder [numCards1 + 1 .. numCards1 + numCards2]
        tier3Deck = zipWith (\card i -> card & cardId .~ CardId i) tier3CardOrder [numCards1 + numCards2 + 1 .. numCards1 + numCards2 + numCards3]
    playNobles <- take (n+1) <$> shuffleM nobleSet
    if n < 2 || n > 4
    then
        pure Nothing
    else
        pure . Just $ GameState
            { _numPlayers = n
            , _playerStates = Vector.replicate n $ PlayerState
                { _heldChips = Map.empty
                , _ownedCards = []
                , _ownedCardCounts = Map.empty
                , _reservedCards = []
                , _ownedNobles = []
                , _currentVP = 0
                }
            , _availableChips =
                case n of
                    2 -> Map.fromList
                       [ (Basic Green, 4)
                       , (Basic Blue, 4)
                       , (Basic Red, 4)
                       , (Basic White, 4)
                       , (Basic Black, 4)
                       , (Gold, 5)
                       ]
                    3 -> Map.fromList
                        [ (Basic Green, 5)
                        , (Basic Blue, 5)
                        , (Basic Red, 5)
                        , (Basic White, 5)
                        , (Basic Black, 5)
                        , (Gold, 5)
                        ]
                    4 -> Map.fromList
                        [ (Basic Green, 7)
                        , (Basic Blue, 7)
                        , (Basic Red, 7)
                        , (Basic White, 7)
                        , (Basic Black, 7)
                        , (Gold, 5)
                        ]
            , _availableNobles = playNobles
            , _tier1State = TierState
                { _availableCards = take 4 tier1Deck
                , _tierDeck = drop 4 tier1Deck
                }
            , _tier2State = TierState
                { _availableCards = take 4 tier2Deck
                , _tierDeck = drop 4 tier2Deck
                }
            , _tier3State = TierState
                { _availableCards = take 4 tier3Deck
                , _tierDeck = drop 4 tier3Deck
                }
            , _currentRequest = ActionRequest
                { _requestPlayer = 0
                , _requestType = TurnRequest
                }
            }

viewPlayer :: PlayerState -> PlayerView
viewPlayer p =
    PlayerView
        { _pvHeldChips = p^.heldChips
        , _pvOwnedCards = p^.ownedCards
        , _pvOwnedCardCounts = p^.ownedCardCounts
        , _pvReservedCardCount = length (p^.reservedCards)
        , _pvOwnedNobles = p^.ownedNobles
        , _pvCurrentVP = p^.currentVP
        }

viewTier :: TierState -> TierView
viewTier t =
    TierView
        { _tvAvailableCards = t^.availableCards
        , _tvDeckCount = length (t^.tierDeck)
        }

viewGame :: Int -> GameState -> GameView
viewGame pos g =
    GameView
        { _gvNumPlayers = g^.numPlayers
        , _gvPlayerPosition = pos
        , _gvPlayerState = (g^.playerStates) Vector.! pos
        , _gvOpponentViews = map (\i -> viewPlayer ((g^.playerStates) Vector.! i)) ([pos+1 .. g^.numPlayers-1] <> [0 .. pos-1])
        , _gvAvailableChips = g^.availableChips
        , _gvAvailableNobles = g^.availableNobles
        , _gvTier1View = viewTier (g^.tier1State)
        , _gvTier2View = viewTier (g^.tier2State)
        , _gvTier3View = viewTier (g^.tier3State)
        , _gvCurrentRequest = g^.currentRequest
        }
