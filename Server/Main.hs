{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad (forever, guard, when)
import Control.Monad.State (runState)
import Data.Aeson hiding ((.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy
import Data.Foldable
import qualified Data.Map as Map
import Data.Monoid
import Data.String
import qualified Data.Text as Text
import Data.Time.Clock
import System.FilePath
import System.IO.Error
import System.Random.Shuffle

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

import Lucid hiding (for_)

import Splendor.Types hiding (id_)
import Splendor.Rules
import Server.GameLogs
import Server.Identifier
import Server.Types

work :: TVar ServerState -> ServerRequest RequestData -> IO ServerResponse
work svar req = do
    curTime <- getCurrentTime
    case req^.requestData of
        GameAction gameKey act -> do
            servState <- readTVarIO svar
            case Map.lookup gameKey (servState^.instances) of
                Nothing -> pure $ ErrorResponse "No such game"
                Just inst -> do
                    case inst^.details of
                        WaitingInstance {} -> pure $ ErrorResponse "Game isn't started"
                        CompletedInstance {} -> pure $ ErrorResponse "Game is already complete"
                        RunningInstance { _playerKeys = instKeys, _runningGame = instRG } -> do
                            case Map.lookup (req^.playerKey) instKeys of
                                Nothing -> pure $ ErrorResponse "Requesting player is not in the game"
                                Just idx -> do
                                    case runAction idx act (instRG^.gameState) of
                                        Nothing -> pure $ ErrorResponse "Invalid game action"
                                        Just (res, gs') -> do
                                            case res of
                                                Nothing -> do
                                                    atomically $ modifyTVar svar (\servState -> servState
                                                        & instances . ix gameKey . details . runningGame . gameState .~ gs'
                                                        & instances . ix gameKey . lastUpdated .~ curTime
                                                        )
                                                    pure $ OkResponse (toJSON ())
                                                Just winners -> do
                                                    let completeGame = instRG { _gameState = gs' }
                                                    atomically . modifyTVar svar $
                                                        instances . at gameKey .~ Just (Instance
                                                            { _name = inst^.name
                                                            , _lastUpdated = curTime
                                                            , _details = CompletedInstance
                                                                { _playerKeys = instKeys
                                                                , _completedGame = completeGame
                                                                , _result = winners
                                                                }
                                                            })
                                                    writeGame gameKey completeGame winners curTime
                                                    pure $ OkResponse (toJSON ())
        NewLobby pInfo params -> do
            let pInfo' =
                    pInfo & displayName %~ Text.take 100
            servState <- atomically $ readTVar svar
            if any (\lob -> lob^?details.ownerKey == Just (req^.playerKey)) (servState^.instances)
            then
                pure $ ErrorResponse "Player already owns an unstarted game"
            else do
                newInstanceId <- newIdentifier
                atomically $ do
                    modifyTVar svar $
                        instances . at newInstanceId .~ Just (Instance
                            { _lastUpdated = curTime
                            , _name = if Text.null (params^.name)
                                then (\n ->
                                    if Text.length n > 20
                                    then Text.take 20 n <> "...'s game"
                                    else n <> "'s game"
                                    ) (pInfo'^.displayName)
                                else Text.take 100 (params^.name)
                            , _details = WaitingInstance
                                { _waitingPlayers = [(req^.playerKey, pInfo')]
                                , _maxPlayers = params^.maxPlayers
                                , _ownerKey = req^.playerKey
                                }
                            })
                pure $ OkResponse (toJSON newInstanceId)
        JoinLobby lobbyKey pInfo -> do
            let pInfo' = pInfo & displayName %~ Text.take 100
            curTime <- getCurrentTime
            atomically $ do
                servState <- readTVar svar
                case servState^.instances.at lobbyKey of
                    Nothing -> pure $ ErrorResponse "No such game"
                    Just (inst@Instance{_details = WaitingInstance { _maxPlayers = m }}) -> do
                        success <- do
                            s <- readTVar svar
                            let (a, s') = flip runState s $ do
                                    currentPlayers <- use (instances . ix lobbyKey . details . waitingPlayers)
                                    if any (\(key, _) -> key == req^.playerKey) currentPlayers
                                    then pure True
                                    else if (length currentPlayers < m)
                                        then do
                                            instances . ix lobbyKey . details . waitingPlayers %= addPlayer (req^.playerKey) pInfo'
                                            instances . ix lobbyKey . lastUpdated .= curTime
                                            pure True
                                        else pure False
                            writeTVar svar s'
                            pure a
                        if success
                        then pure $ OkResponse (toJSON ())
                        else pure $ ErrorResponse "Game is full"
                    Just _ -> pure $ ErrorResponse "Game already started"
        LeaveLobby lobbyKey -> do
            curTime <- getCurrentTime
            atomically $ do
                modifyTVar svar $
                    instances . at lobbyKey %~ (\case
                        Just (inst@(Instance
                                { _details = WaitingInstance
                                      { _waitingPlayers = players
                                      , _ownerKey = owner
                                      }
                                })) ->
                            if owner == req^.playerKey
                            then Nothing
                            else Just $ inst
                                & lastUpdated .~ curTime
                                & details . waitingPlayers %~ filter ((/= req^.playerKey) . fst)
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
                Just (Instance { _name = n, _details = WaitingInstance { _waitingPlayers = players, _ownerKey = owner }}) -> do
                    if req^.playerKey == owner
                    then do
                        playerOrder <- shuffleM players
                        gameStart <- initState (length playerOrder)
                        case gameStart of
                            Nothing -> pure $ ErrorResponse "Error attempting to start game"
                            Just gs -> atomically $ do
                                let inst = Instance
                                        { _name = n
                                        , _lastUpdated = curTime
                                        , _details = RunningInstance
                                            { _playerKeys = Map.fromList (zip (map fst playerOrder) [0..])
                                            , _runningGame = RunningGame
                                                { _players = Map.fromList (zip [0..] (map snd playerOrder))
                                                , _version = 0
                                                , _gameState = gs
                                                }
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

culler :: TVar ServerState -> IO ()
culler svar = forever $ do
    curTime <- getCurrentTime
    servState <- readTVarIO svar
    for_ (Map.toList $ servState^.instances) $ \(instKey, inst) -> do
        let updatedTime = inst^.lastUpdated        
        when (diffUTCTime curTime updatedTime > 3600) $ do
            atomically $ do
                modifyTVar svar $ instances.at instKey .~ Nothing
    threadDelay (60*10^6)

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
    case inst^.details of
        WaitingInstance { _waitingPlayers = players, _maxPlayers = m } ->
            InstanceSummary
                { _name = inst^.name
                , _players = map snd players
                , _state = Waiting 
                , _maxPlayers = Just m
                }
        RunningInstance { _runningGame = rg } ->
            InstanceSummary
                { _name = inst^.name
                , _players = toList (rg^.players)
                , _state = Running
                , _maxPlayers = Nothing
                }
        CompletedInstance { _completedGame = rg } ->
            InstanceSummary
                { _name = inst^.name
                , _players = toList (rg^.players)
                , _state = Completed
                , _maxPlayers = Nothing
                }

viewInstance :: String -> Instance -> Maybe InstanceView
viewInstance playerKey inst =
    case inst^.details of
        WaitingInstance { _waitingPlayers = players, _maxPlayers = m } ->
            Just $ InstanceView
                { _name = inst^.name
                , _details = WaitingInstanceView
                    { _waitingPlayers = map snd players
                    , _maxPlayers = m
                    }
                }
        RunningInstance
                { _playerKeys = keyMap
                , _runningGame = rg
                } ->
            case keyMap ^. at playerKey of
                Nothing -> Nothing
                Just idx ->
                    Just $ InstanceView
                        { _name = inst^.name
                        , _details = RunningInstanceView
                            { _runningGame = viewGame idx <$> rg
                            }
                        }
        CompletedInstance
                { _completedGame = cg
                , _result = res
                } ->
            Just $ InstanceView
                { _name = inst^.name
                , _details = CompletedInstanceView
                    { _completedGame = cg
                    , _result = res
                    }
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
    forkIO $ culler svar
    pure $ \request -> do
        if requestMethod request == methodGet
        then do
            case pathInfo request of
                [] -> pure $ responseLBS status200 [("Content-Type", "text/html")] (renderBS mainPage)
                ["client.js"] -> serveFile "client/client.js" request
                ["client.css"] -> serveFile "client/client.css" request
                "games":_ -> archiveR request
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
