module Components.PlayerInfo
    ( PlayerInfoAction()
    , defaultName
    , initializePlayerInfo
    , spec
    ) where

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import Unsafe.Coerce (unsafeCoerce)

import Browser.LocalStorage
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Trans (lift)
import Data.Generic
import Data.Maybe

import Splendor.Types

defaultName :: String
defaultName = "Anonymous"

data PlayerInfoAction =
    SetDisplayName String

data StorageKey a = PlayerInfoKey
derive instance genericStorageKey :: Generic (StorageKey a)

playerInfoKey :: StorageKey PlayerInfo
playerInfoKey = PlayerInfoKey

initializePlayerInfo :: forall e. Eff (storage :: STORAGE | e) PlayerInfo
initializePlayerInfo = do
    storedInfo <- localStorage.getItem playerInfoKey
    case storedInfo of
        Nothing -> pure $ PlayerInfo
            { displayName: defaultName
            }
        Just info -> pure info

render :: T.Render PlayerInfo _ _
render dispatch _ (PlayerInfo pinfo) _ =
    [ R.input
        [ RP.placeholder "Your Name"
        , RP.value pinfo.displayName
        , RP.onKeyUp \e -> dispatch (SetDisplayName (unsafeCoerce e).target.value)
        , RP.onChange \e -> dispatch (SetDisplayName (unsafeCoerce e).target.value)
        ] []
    ]

performAction :: forall e. T.PerformAction (storage :: STORAGE | e) PlayerInfo _ PlayerInfoAction
performAction a _ _ =
    case a of
        SetDisplayName name -> do
            s' <- T.cotransform (\(PlayerInfo pinfo) -> PlayerInfo $ pinfo { displayName = name })
            case s' of
                Nothing -> pure unit
                Just pinfo' -> do
                    lift <<< liftEff $ localStorage.setItem playerInfoKey pinfo'

spec :: forall e. T.Spec (storage :: STORAGE | e) PlayerInfo _ PlayerInfoAction
spec = T.simpleSpec performAction render
