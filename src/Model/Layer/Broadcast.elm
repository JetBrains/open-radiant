module Model.Layer.Broadcast exposing (..)

import Model.Product exposing (Product)

-- type ToAllMsg
--     = IFeelLucky
--     | ChangeProduct Product
--     | Resize (Int, Int)
--     -- | Bang
--     -- | ChangeMode AppMode


-- type ToTargetMsg
--     = TurnOn
--     | TurnOff


-- type Msg
--     = ToAll ToAllMsg
--     | ToTarget Int ToTargetMsg


type Msg
    = IFeelLucky
    | ChangeProduct Product
    | Resize (Int, Int)
    | TurnOn
    | TurnOff

