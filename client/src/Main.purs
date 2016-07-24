module Main where

import Prelude
import Control.Monad.Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random

import Browser.LocalStorage
import Thermite as T
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import React as R
import ReactDOM as RDOM
import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import Partial.Unsafe (unsafePartial)

import Components.Client as Client
import Splendor.Types

main :: forall e. Eff (dom :: DOM, storage :: STORAGE, random :: RANDOM | e) Unit
main = void do
    initialClientState <- Client.initializeState
    let reactSpec = (T.createReactSpec Client.spec initialClientState).spec
    let reactSpec' = reactSpec
            { componentWillMount = \rthis -> void $ launchAff (Client.backgroundWork rthis) }
    let component = R.createClass reactSpec'
    document <- DOM.window >>= DOM.document
    container <- unsafePartial (fromJust <<< toMaybe <$> DOM.querySelector "#client" (DOM.htmlDocumentToParentNode document))
    RDOM.render (R.createFactory component {}) container
