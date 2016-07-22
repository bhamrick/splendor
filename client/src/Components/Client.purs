module Components.Client where

import Prelude
import Thermite as T

import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM

import Browser.LocalStorage
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Data.Maybe
import Data.Int as Int
import Data.Generic

type ClientState =
    { clientKey :: String }

data ClientAction =
    DoNothing

newKey :: forall e. Eff (random :: RANDOM | e) String
newKey = go 40
    where
    go n =
        if n == 0
        then pure ""
        else do
            c <- randomInt 0 15
            rest <- go (n-1)
            pure $ (Int.toStringAs Int.hexadecimal c) <> rest

newtype ClientKey = ClientKey String
derive instance genericClientKey :: Generic ClientKey

data StorageKey a = ClientKeyKey
derive instance genericStorageKey :: Generic (StorageKey a)

clientKeyKey :: StorageKey ClientKey
clientKeyKey = ClientKeyKey

initializeState :: forall e. Eff (storage :: STORAGE, random :: RANDOM | e) ClientState
initializeState = do
    storedKey <- localStorage.getItem clientKeyKey
    case storedKey of
        Nothing -> do
            key <- newKey
            localStorage.setItem clientKeyKey (ClientKey key)
            pure $ {clientKey: key}
        Just (ClientKey key) -> do
            pure $ {clientKey: key}

render :: T.Render ClientState _ _
render dipsatch _ state _ =
    [ R.p'
        [ R.text "Client Key: "
        , R.text $ state.clientKey
        ]
    ]

performAction :: T.PerformAction _ ClientState _ ClientAction
performAction DoNothing _ _ = void (T.cotransform id)

spec :: T.Spec _ ClientState _ ClientAction
spec = T.simpleSpec performAction render
