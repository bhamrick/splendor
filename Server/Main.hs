{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.STM
import Control.Lens
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import System.Random.Shuffle

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

import Lucid

import Splendor.Types
import Splendor.Rules
import Server.Identifier
import Server.Types

work :: TVar (ServerState GameState) -> ServerRequest RequestData -> IO Response
work svar req = do
    case req^.requestData of
        GameAction gameKey act -> atomically $ do
            servState <- readTVar svar
            case Map.lookup gameKey (servState^.instances) of
                Nothing -> pure $ responseLBS status400 [("Content-Type", "text/plain")] ""
                Just inst -> do
                    case Map.lookup (req^.playerKey) (inst^.playerKeys) of
                        Nothing -> pure $ responseLBS status400 [("Content-Type", "text/plain")] ""
                        Just idx -> do
                            case runAction idx act (inst^.runningGame.gameState) of
                                Nothing -> pure $ responseLBS status400 [("Content-Type", "text/plain")] ""
                                Just (res, gs') -> do
                                    writeTVar svar (servState & instances . ix gameKey . runningGame . gameState .~ gs')
                                    pure $ responseLBS status200 [("Content-Type", "application/json")] (encode (res, gs'))
        NewLobby pInfo -> do
            newLobbyId <- newIdentifier
            atomically $ do
                modifyTVar svar $
                    lobbies . at newLobbyId .~ Just (Lobby
                        { _waitingPlayers = [(req^.playerKey, pInfo)]
                        , _ownerKey = req^.playerKey
                        })
            pure $ responseLBS status200 [("Content-Type", "text/plain")] ""
        JoinLobby lobbyKey pInfo -> do
            newLobbyId <- newIdentifier
            atomically $ do
                modifyTVar svar $
                    lobbies . ix newLobbyId . waitingPlayers %~ (:) (req^.playerKey, pInfo)
            pure $ responseLBS status200 [("Content-Type", "text/plain")] ""
        ListLobbies -> do
            servState <- readTVarIO svar
            pure $ responseLBS status200 [("Content-Type", "application/json")] (encode (fmap (map snd . view waitingPlayers) (servState^.lobbies)))
        GetGameState gameKey -> do
            servState <- readTVarIO svar
            let maybeInst = servState^?instances.ix gameKey
            case maybeInst of
                Nothing -> pure $ responseLBS status404 [("Content-Type", "text/plain")] ""
                Just inst -> do
                    case inst^.playerKeys.at (req^.playerKey) of
                        Nothing -> pure $ responseLBS status400 [("Content-Type", "text/plain")] ""
                        Just pos -> pure $ responseLBS status200 [("Content-Type", "application/json")] (encode (fmap (viewGame pos) (inst^.runningGame)))
        StartGame lobbyKey -> do
            lobbyData <- do
                servState <- readTVarIO svar
                pure (servState ^. lobbies . at lobbyKey)
            case lobbyData of
                Nothing -> pure $ responseLBS status200 [("Content-Type", "text/plain")] ""
                Just ldat -> do
                    if req^.playerKey == ldat^.ownerKey
                    then do
                        playerOrder <- shuffleM (ldat ^. waitingPlayers)
                        gameStart <- initState (length playerOrder)
                        case gameStart of
                            Nothing -> pure $ responseLBS status200 [("Content-Type", "text/plain")] ""
                            Just gs -> atomically $ do
                                let inst = Instance
                                        { _playerKeys = Map.fromList (zip (map fst playerOrder) [0..])
                                        , _runningGame = RunningGame
                                            { _players = Map.fromList (zip [0..] (map snd playerOrder))
                                            , _version = 0
                                            , _gameState = gs
                                            }
                                        }
                                modifyTVar svar $ \servState -> servState
                                    & lobbies . at lobbyKey .~ Nothing
                                    & instances . at lobbyKey .~ Just inst
                                pure $ responseLBS status200 [("Content-Type", "text/plain")] ""
                    else
                        pure $ responseLBS status200 [("Content-Type", "text/plain")] ""

mainPage :: Html()
mainPage = do
    doctype_
    head_ $ do
        title_ "Splendor Server"
    body_ $ do
        "Hello"

serverApplication :: IO (Request -> IO Response)
serverApplication = do
    svar <- newTVarIO $ ServerState
        { _instances = Map.empty
        , _lobbies = Map.empty
        }
    pure $ \request -> do
        if requestMethod request == methodGet
        then do
            pure $ responseLBS status200 [("Content-Type", "text/html")] (renderBS mainPage)
        else do
            bod <- lazyRequestBody request
            case decode bod of
                Nothing -> pure $ responseLBS status400 [("Content-Type", "text/plain")] ""
                Just req -> work svar req

main :: IO ()
main = do
    app <- serverApplication
    runEnv 8080 (\req cb -> app req >>= cb)
