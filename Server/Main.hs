{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad (guard)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy
import Data.Foldable
import qualified Data.Map as Map
import Data.String
import System.FilePath
import System.IO.Error
import System.Random.Shuffle

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

import Lucid

import Splendor.Types
import Splendor.Rules
import Server.Identifier
import Server.Types

work :: TVar ServerState -> ServerRequest RequestData -> IO ServerResponse
work svar req = do
    case req^.requestData of
        GameAction gameKey act -> atomically $ do
            servState <- readTVar svar
            case Map.lookup gameKey (servState^.instances) of
                Nothing -> pure $ ErrorResponse "No such game"
                Just inst -> do
                    case Map.lookup (req^.playerKey) (inst^.playerKeys) of
                        Nothing -> pure $ ErrorResponse "Requesting player is not in the game"
                        Just idx -> do
                            case (inst^?runningGame.gameState) >>= runAction idx act of
                                Nothing -> pure $ ErrorResponse "Invalid game action"
                                Just (res, gs') -> do
                                    writeTVar svar (servState & instances . ix gameKey . runningGame . gameState .~ gs')
                                    pure $ OkResponse (toJSON ())
        NewLobby pInfo -> do
            servState <- atomically $ readTVar svar
            if any (\lob -> lob^.ownerKey == req^.playerKey) (servState^.instances)
            then
                pure $ ErrorResponse "Player already owns an unstarted game"
            else do
                newInstanceId <- newIdentifier
                atomically $ do
                    modifyTVar svar $
                        instances . at newInstanceId .~ Just (WaitingInstance
                            { _waitingPlayers = [(req^.playerKey, pInfo)]
                            , _ownerKey = req^.playerKey
                            })
                pure $ OkResponse (toJSON newInstanceId)
        JoinLobby lobbyKey pInfo -> do
            atomically $ do
                servState <- readTVar svar
                case servState^.instances.at lobbyKey of
                    Nothing -> pure $ ErrorResponse "No such game"
                    Just (inst@WaitingInstance {}) -> do
                        modifyTVar svar $
                            instances . ix lobbyKey . waitingPlayers %~ addPlayer (req^.playerKey) pInfo
                        pure $ OkResponse (toJSON ())
                    Just _ -> pure $ ErrorResponse "Game already started"
        LeaveLobby lobbyKey -> do
            atomically $ do
                modifyTVar svar $
                    instances . at lobbyKey %~ (\case
                        Just (inst@(WaitingInstance
                              { _waitingPlayers = players
                              , _ownerKey = owner
                              })) ->
                            if owner == req^.playerKey
                            then Nothing
                            else Just $ inst & waitingPlayers %~ filter ((/= req^.playerKey) . fst)
                        inst -> inst
                        )
            pure $ OkResponse (toJSON ())
        ListLobbies -> do
            servState <- readTVarIO svar
            pure . OkResponse . toJSON . fmap summarizeInstance $ (servState^.instances)
        GetGameState gameKey -> do
            servState <- readTVarIO svar
            let maybeInst = servState^?instances.ix gameKey
            case maybeInst of
                Nothing -> pure $ ErrorResponse "No such instance"
                Just inst -> do
                    pure $ OkResponse (toJSON (viewInstance (req^.playerKey) inst))
        StartGame instanceKey -> do
            lobbyData <- do
                servState <- readTVarIO svar
                pure (servState ^. instances . at instanceKey)
            case lobbyData of
                Just (WaitingInstance { _waitingPlayers = players, _ownerKey = owner }) -> do
                    if req^.playerKey == owner
                    then do
                        playerOrder <- shuffleM players
                        gameStart <- initState (length playerOrder)
                        case gameStart of
                            Nothing -> pure $ ErrorResponse "Error attempting to start game"
                            Just gs -> atomically $ do
                                let inst = RunningInstance
                                        { _playerKeys = Map.fromList (zip (map fst playerOrder) [0..])
                                        , _runningGame = RunningGame
                                            { _players = Map.fromList (zip [0..] (map snd playerOrder))
                                            , _version = 0
                                            , _gameState = gs
                                            }
                                        }
                                modifyTVar svar $ \servState -> servState
                                    & instances . at instanceKey .~ Just inst
                                pure $ OkResponse (toJSON ())
                    else
                        pure $ ErrorResponse "Non-owner cannot start game"
                Just inst ->
                    pure $ ErrorResponse "Game already started"
                Nothing ->
                    pure $ ErrorResponse "No such game"

addPlayer :: String -> PlayerInfo -> [(String, PlayerInfo)] -> [(String, PlayerInfo)]
addPlayer key pinfo players =
    if any ((== key) . fst) players
    then
        map (\(k, v) ->
            if k == key
            then (k, pinfo)
            else (k, v)
            ) players
    else players ++ [(key, pinfo)]

summarizeInstance :: Instance -> InstanceSummary
summarizeInstance inst =
    case inst of
        WaitingInstance { _waitingPlayers = players } ->
            InstanceSummary
                { _isPlayers = map snd players
                , _isState = Waiting
                }
        RunningInstance { _runningGame = rg } ->
            InstanceSummary
                { _isPlayers = toList (rg^.players)
                , _isState = Running
                }
        CompletedInstance { _completedGame = rg } ->
            InstanceSummary
                { _isPlayers = toList (rg^.players)
                , _isState = Completed
                }

viewInstance :: String -> Instance -> Maybe InstanceView
viewInstance playerKey inst =
    case inst of
        WaitingInstance { _waitingPlayers = players } ->
            Just $ WaitingInstanceView
                { _ivWaitingPlayers = map snd players
                }
        RunningInstance
                { _playerKeys = keyMap
                , _runningGame = rg
                } ->
            case keyMap ^. at playerKey of
                Nothing -> Nothing
                Just idx ->
                    Just $ RunningInstanceView
                        { _ivRunningGame = viewGame idx <$> rg
                        }
        CompletedInstance
                { _completedGame = cg
                , _result = res
                } ->
            Just $ CompletedInstanceView
                { _ivCompletedGame = cg
                , _ivResult = res
                }

mainPage :: Html ()
mainPage = do
    doctype_
    head_ $ do
        title_ "Splendor"
        link_ [rel_ "stylesheet", type_ "text/css", href_ "client.css"]
    body_ $ do
        div_ [id_ "client"] $ do
            script_ [src_ "client.js"] ("" :: ByteString)

contentType :: IsString a => String -> a
contentType ext = case ext of
    ".css"  -> "text/css"
    ".gif"  -> "image/gif"
    ".jpeg" -> "image/jpeg"
    ".jpg"  -> "image/jpeg"
    ".js"   -> "application/javascript"
    ".json" -> "application/json"
    ".pdf"  -> "application/pdf"
    ".png"  -> "image/png"
    ".ps"   -> "application/postscript"
    ".txt"  -> "text/plain"
    _       -> "application/unknown"

serveFile :: FilePath -> Request -> IO Response
serveFile path req = do
    r <- tryJust (guard . isDoesNotExistError) $ Data.ByteString.Lazy.readFile path
    return $ case r of
        Left error      -> responseLBS status404 [("Content-Type", "application/json")] "[]"
        Right content   -> responseLBS status200 [("Content-Type", contentType . takeExtension $ path)] content

serverApplication :: IO (Request -> IO Response)
serverApplication = do
    svar <- newTVarIO $ ServerState
        { _instances = Map.empty
        }
    pure $ \request -> do
        if requestMethod request == methodGet
        then do
            case pathInfo request of
                [] -> pure $ responseLBS status200 [("Content-Type", "text/html")] (renderBS mainPage)
                ["client.js"] -> serveFile "client/client.js" request
                ["client.css"] -> serveFile "client/client.css" request
                _ -> pure $ responseLBS status404 [("Content-Type", "application/json")] "[]"
        else do
            bod <- lazyRequestBody request
            case decode bod of
                Nothing -> pure $ responseLBS status400 [("Content-Type", "application/json")] "[]"
                Just req -> do
                    resp <- work svar req
                    pure $ responseLBS status200 [("Content-Type", "application/json")] (encode resp)

main :: IO ()
main = do
    app <- serverApplication
    runEnv 8080 (\req cb -> app req >>= cb)
