module Model.SizeRule exposing
    ( ViewportSize(..)
    , SizeRule(..), default, encode, decode
    , getPresetLabel, getPresetSize, getRuleSize, getRuleSizeOrZeroes
    , getSizePresets
    , SizePresetCode, encodePreset, decodePreset
    , toTuple, toRecord
    )


import Array


import Model.AppMode exposing (..)


type ViewportSize = ViewportSize Int Int


type alias SizePresetCode = String


type SizeRule
    = FromPreset SizePreset
    | UseViewport ViewportSize
    | Custom Int Int
    | Dimensionless


type Factor
    = Single
    | Double


type SizePreset
    = ProductCard Factor
    | ProductSplash Factor
    | Newsletter Factor
    | Twitter
    | Facebook
    | Baidu Int Int
    | Ad Int Int
    | Instagram
    | LinkedIn
    | WebpagePreview
    | BlogHeader Factor
    | BlogFooter Factor
    | LandingPage
    | Wallpaper Int Int


default : SizeRule
default = Dimensionless


getSizePresets : AppMode -> List SizePreset
getSizePresets mode =
    case mode of
        Release ->
            [ ProductCard Single
            , ProductCard Double
            , ProductSplash Single
            , ProductSplash Double
            , Newsletter Single
            , Newsletter Double
            , Twitter
            , Facebook
            , WebpagePreview
            , BlogHeader Single
            , BlogHeader Double
            , BlogFooter Single
            , BlogFooter Double
            , LandingPage
            ]
        Ads ->
            (
                [ ( 120, 600 )
                , ( 125, 125 )
                , ( 130, 100 )
                , ( 180, 150 )
                , ( 200, 125 )
                , ( 200, 200 )
                , ( 220, 250 )
                , ( 250, 250 )
                , ( 260, 200 )
                , ( 300, 250 )
                , ( 320, 100 )
                , ( 320, 50 )
                , ( 336, 280 )
                , ( 468, 60 )
                , ( 160, 60 )
                , ( 300, 600 )
                , ( 728, 90 )
                , ( 800, 320 )
                , ( 300, 600 )
                , ( 970, 250 )
                , ( 970, 90 )
                ] |> List.map (\(w, h) -> Ad w h)
            ) ++ (
                [ ( 960, 60 )
                , ( 728, 90 )
                , ( 468, 60 )
                , ( 200, 200 )
                , ( 960, 60 )
                , ( 640, 60 )
                , ( 580, 90 )
                , ( 460, 60 )
                , ( 300, 250 )
                , ( 336, 280 )
                ] |> List.map (\(w, h) -> Baidu w h)
            ) ++ (
                [ Facebook
                , Twitter
                , Instagram
                , LinkedIn
                ]
            )
        TronUi tronMode -> getSizePresets tronMode
        _ -> -- return Wallpaper size preset for eveything else
            [ ( 2560, 1440 )
            , ( 1920, 1200 )
            , ( 1920, 1080 )
            , ( 1680, 1050 )
            , ( 1536, 864)
            , ( 1440, 900 )
            , ( 1366, 768 )
            ] |> List.map (\(w, h) -> Wallpaper w h)


getRuleSize : SizeRule -> Maybe ( Int, Int )
getRuleSize rule =
    case rule of
        FromPreset preset -> Just <| getPresetSize preset
        UseViewport (ViewportSize w h) -> Just (w, h)
        Custom w h -> Just (w, h)
        Dimensionless -> Nothing


getRuleSizeOrZeroes : SizeRule -> ( Int, Int )
getRuleSizeOrZeroes rule =
    getRuleSize rule |> Maybe.withDefault (0, 0)


getPresetSize : SizePreset -> ( Int, Int )
getPresetSize preset =
    let
        applyFactor factor ( w, h ) =
            case factor of
                Single -> ( w, h )
                Double -> ( w * 2, h * 2 )
    in
        case preset of
            ProductCard factor -> ( 480, 297 ) |> applyFactor factor
            ProductSplash factor -> ( 640, 400 ) |> applyFactor factor
            Newsletter factor -> ( 650, 170 ) |> applyFactor factor
            Twitter -> ( 800, 418 )
            Facebook -> ( 1200, 628 )
            WebpagePreview -> ( 1200, 800 )
            BlogHeader factor -> ( 800, 400 ) |> applyFactor factor
            BlogFooter factor -> ( 800, 155 ) |> applyFactor factor
            LandingPage -> ( 2850, 1200 )
            Instagram -> ( 1080, 1080 )
            LinkedIn -> ( 1200, 627 )
            Baidu w h -> ( w, h )
            Ad w h -> ( w, h )
            Wallpaper w h -> ( w, h )


getPresetLabel : SizePreset -> String
getPresetLabel preset =
    let
        sizeStr = getPresetSize preset |>
            \(w, h) -> String.fromInt w ++ "x" ++ String.fromInt h
        factorStr factor =
            case factor of
                Single -> ""
                Double -> "@2x"
    in
        sizeStr ++ case preset of
            ProductCard factor -> " prodcard" ++ factorStr factor
            ProductSplash factor -> " spl" ++ factorStr factor
            Newsletter factor -> " nwlt" ++ factorStr factor
            Twitter -> " tw"
            Facebook -> " fb"
            WebpagePreview -> " wprev"
            BlogHeader factor -> " blog" ++ factorStr factor
            BlogFooter factor -> " bfoot" ++ factorStr factor
            LandingPage -> " landg"
            Instagram -> " in"
            LinkedIn -> " ln"
            Baidu w h -> " baidu"
            Ad w h -> ""
            Wallpaper w h -> ""


encodePreset : SizePreset -> SizePresetCode
encodePreset preset =
   let
        sizeStr w h = String.fromInt w ++ "x" ++ String.fromInt h
        factorStr factor =
            case factor of
                Single -> "1"
                Double -> "2"
    in
        case preset of
            ProductCard factor -> "PCx" ++ factorStr factor
            ProductSplash factor -> "SPx" ++ factorStr factor
            Newsletter factor -> "NLx" ++ factorStr factor
            Twitter -> "TW"
            Facebook -> "FB"
            WebpagePreview -> "WB"
            BlogHeader factor -> "BHx" ++ factorStr factor
            BlogFooter factor -> "BFx" ++ factorStr factor
            LandingPage -> "LP"
            Instagram -> "IN"
            LinkedIn -> "LN"
            Baidu w h -> "BA:" ++ sizeStr w h
            Ad w h -> "AD:" ++ sizeStr w h
            Wallpaper w h -> "WP:" ++ sizeStr w h


decodePreset : SizePresetCode -> Maybe SizePreset
decodePreset presetStr =
    let
        presetCode = String.left 2 presetStr
        decodeFactor nStr =
            case nStr of
                "1" -> Just Single
                "2" -> Just Double
                _ -> Nothing
        withFactor f =
            String.dropLeft 3 presetStr
                |> decodeFactor
                |> Maybe.map f
        withSize f =
            case String.dropLeft 3 presetStr |> String.split "x" of
                wStr::hStr::_ ->
                    case ( String.toInt wStr, String.toInt hStr ) of
                        ( Just w, Just h ) -> f w h |> Just
                        _ -> Nothing
                _ -> Nothing
        decodeByCode code =
            case code of
                "PC" -> withFactor ProductCard
                "SP" -> withFactor ProductSplash
                "NL" -> withFactor Newsletter
                "TW" -> Just Twitter
                "FB" -> Just Facebook
                "WB" -> Just WebpagePreview
                "BH" -> withFactor BlogHeader
                "BF" -> withFactor BlogFooter
                "LP" -> Just LandingPage
                "IN" -> Just Instagram
                "LN" -> Just LinkedIn
                "BA" -> withSize Baidu
                "AD" -> withSize Ad
                "WP" -> withSize Wallpaper
                _ -> Nothing
    in
       decodeByCode presetCode


encode : SizeRule -> String
encode rule =
    case rule of
        Custom w h -> "custom:" ++ String.fromInt w ++ "x" ++ String.fromInt h
        FromPreset preset -> "preset:" ++ encodePreset preset
        UseViewport (ViewportSize w h) -> "viewport:" ++ String.fromInt w ++ "x" ++ String.fromInt h
        Dimensionless -> "dimensionless"


decode : String -> Result String SizeRule
decode str =
    let
        decodeSize f w_and_h defaultWidth defaultHeight =
            case String.split "x" w_and_h of
                wStr::hStr::_ ->
                    case ( String.toInt wStr, String.toInt hStr ) of
                        ( Just w, Just h ) -> f w h
                        _ -> f defaultWidth defaultHeight
                _ -> f defaultWidth defaultHeight
    in case String.split ":" str of
        "custom"::w_and_h::_ ->
            Ok <| decodeSize Custom w_and_h -1 -1
        "preset"::presetStr::w_and_h::_ ->
            decodePreset (presetStr ++ ":" ++ w_and_h)
                |> Result.fromMaybe str
                |> Result.map FromPreset
        "preset"::presetStr::_ ->
            decodePreset presetStr
                |> Result.fromMaybe str
                |> Result.map FromPreset
        "viewport"::w_and_h::_ ->
            Ok <| decodeSize (\w h -> UseViewport (ViewportSize w h)) w_and_h -1 -1
        "dimensionless"::_ -> Ok Dimensionless
        _ -> Err str


toTuple : SizeRule -> ( Int, Int )
toTuple = getRuleSizeOrZeroes


toRecord : SizeRule -> { width : Int, height : Int }
toRecord sizeRule =
    case getRuleSizeOrZeroes sizeRule of
        ( width, height ) -> { width = width, height = height }
