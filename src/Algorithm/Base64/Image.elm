module Algorithm.Base64.Image exposing (Order(..), Options, defaultOptions)

{- From https://package.elm-lang.org/packages/justgook/elm-image-encode/latest/ -}

{-|
# Options
@docs defaultOptions, Options, Order
-}


{-| Pixel render order in image
-}
type Order
    = RightDown
    | RightUp
    | LeftDown
    | LeftUp


{-| -}
type alias Options a =
    { a
        | defaultColor : Int
        , order : Order
    }


{-|
    { defaultColor = 0x00FFFF00
    , order = RightDown
    }
-}
defaultOptions : Options {}
defaultOptions =
    { defaultColor = 0x00FFFF00
    , order = RightDown
    }
