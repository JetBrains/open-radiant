module Model.Layer.Broadcast exposing (..)

import Model.Product exposing (Product)


type Msg
    = IFeelLucky
    | ChangeProduct Product
    | Resize (Int, Int)
    | TurnOn
    | TurnOff
    -- TODO: Animate, TurnOn, TurnOff,.etc.


-- type Global
--     = IFeelLucky

-- type Local
--     = TurnOn
