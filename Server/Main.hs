module Server.Main where

import Control.Concurrent.STM
import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Splendor.Types
import Splendor.Rules
import Server.Types

work :: TVar (ServerState GameState) -> ServerRequest RequestData -> IO Response
work svar req = do
    case req^.requestData of
        GameAction act -> do
            undefined
        NewLobby -> do
            undefined
        JoinLobby lobbyKey pInfo -> do
            undefined
        ListLobbies -> do
            undefined
        GetGameState String -> do
            undefined
