module Components.NewGameParams where

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import Unsafe.Coerce (unsafeCoerce)

import Data.Int as Int
import Data.Maybe

import Splendor.Types

defaultParams :: NewGameParams
defaultParams = NewGameParams
    { name: ""
    , maxPlayers: 4
    }

data NewGameParamsAction
    = SetGameName String
    | SetMaxPlayers String

render :: T.Render NewGameParams _ _
render dispatch _ (NewGameParams p) _ =
    [ R.input
        [ RP.placeholder "Game Name"
        , RP.value p.name
        , RP.onKeyUp \e -> dispatch (SetGameName (unsafeCoerce e).target.value)
        , RP.onChange \e -> dispatch (SetGameName (unsafeCoerce e).target.value)
        ] []
    , R.div
        [ RP.className "maxPlayers" ]
        [ R.text "Max players: "
        , R.select
            [ RP.onChange \e -> dispatch (SetMaxPlayers (unsafeCoerce e).target.value)
            ]
            (map (\n ->
                R.option
                    (if p.maxPlayers == n
                    then [ RP.value (show n), RP.selected "selected" ]
                    else [ RP.value (show n) ])
                    [ R.text (show n) ]
                ) [2, 3, 4]
            )
        ]
    ]

performAction :: T.PerformAction _ NewGameParams _ NewGameParamsAction
performAction a _ _ =
    case a of
        SetGameName n -> do
            void $ T.cotransform (\(NewGameParams params) -> NewGameParams (params { name = n }))
        SetMaxPlayers pStr ->
            case Int.fromString pStr of
                Nothing -> pure unit
                Just p -> void $ T.cotransform (\(NewGameParams params) -> NewGameParams (params { maxPlayers = p }))

spec :: T.Spec _ NewGameParams _ NewGameParamsAction
spec = T.simpleSpec performAction render
