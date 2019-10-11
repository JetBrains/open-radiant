module Model.Layer.Broadcast exposing (..)

import Model.Product exposing (Product)


type Msg
    = IFeelLucky
    | ChangeProduct Product
    | Resize (Int, Int)
    | TurnOn
    | TurnOff
    -- TODO: Animate, TurnOn, TurnOff,.etc.
    -- = ToAll ToAllMsg
    -- | ToTarget TargetMsg


-- type ToAllMsg
--     = IFeelLucky

-- type ToTargetMsg
--     = TurnOn
