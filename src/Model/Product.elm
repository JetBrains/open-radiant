module Model.Product exposing
    ( Product(..)
    , ColorId(..)
    , Gradient
    , default
    , allProducts
    , getName
    , decode
    , encode
    , getLogoPath
    , getTextLinePath
    , getCoverTextSize
    , getId
    , ProductId
    , Palette(..)
    , Color
    , transparentPalette
    , getPalette
    , getPaletteColor
    , paletteToList
    , encodePalette
    , decodePalette
    -- , paletteToArray
    , applyPalette
    , emptyGradient
    , encodeGradient
    , decodeGradient
    )


import Array exposing (..)

import Json.Encode as E exposing (..)
import Json.Decode as D exposing (..)

import Gradient as Generic exposing (Gradient)
import Gradient as GenericGradient exposing (encode, decode)
import Gradient as Gradient exposing (Orientation)


type alias Color = String
type alias ProductId = Int


type Palette = Palette Color Color Color -- TODO: include (Maybe Product)?


type alias Gradient =
    { stops: List ( Float, ColorId )
    , orientation: Gradient.Orientation
    }


type ColorId
    = ColorI
    | ColorII
    | ColorIII
    -- TODO: Unknown


type Product
    = JetBrains
    | IntelliJ
    | PhpStorm
    | PyCharm
    | RubyMine
    | WebStorm
    | CLion
    | DataGrip
    | AppCode
    | GoLand
    | ReSharper
    | ReSharperCpp
    | DotCover
    | DotMemory
    | DotPeek
    | DotTrace
    | Rider
    | TeamCity
    | YouTrack
    | Upsource
    | Hub
    | Kotlin
    | MPS


default : Product
default = JetBrains


transparentPalette : Palette
transparentPalette = Palette "rgba(0,0,0,0)" "rgba(0,0,0,0)" "rgba(0,0,0,0)"


allProducts : List Product
allProducts =
    [ JetBrains -- 0
    , IntelliJ -- 1
    , PhpStorm -- 2
    , PyCharm -- 3
    , RubyMine -- 4
    , WebStorm -- 5
    , CLion -- 6
    , DataGrip -- 7
    , AppCode -- 8
    , GoLand -- 9
    , ReSharper -- 10
    , ReSharperCpp -- 11
    , DotCover -- 12
    , DotMemory -- 13
    , DotPeek -- 14
    , DotTrace -- 15
    , Rider -- 16
    , TeamCity -- 17
    , YouTrack -- 18
    , Upsource -- 19
    , Hub -- 20
    , Kotlin -- 21
    , MPS -- 22
    ]


-- These were the default ones

-- layerOneConfig.lights.ambient = [ '#000000', '#f45b69' ];
-- layerOneConfig.lights.diffuse = [ '#000000', '#e4fde1' ];

-- layerTwoConfig.lights.ambient = [ '#000000', '#4b4e76' ];
-- layerOneConfig.lights.diffuse = [ '#000000', '#fb4e76' ];


getName : Product -> String
getName product =
    case product of
        JetBrains -> "JetBrains"
        IntelliJ -> "IntelliJ IDEA"
        PhpStorm -> "PhpStorm"
        PyCharm -> "PyCharm"
        RubyMine -> "RubyMine"
        WebStorm -> "WebStorm"
        CLion -> "CLion"
        DataGrip -> "DataGrip"
        AppCode -> "AppCode"
        GoLand -> "GoLand"
        ReSharper -> "ReSharper"
        ReSharperCpp -> "ReSharper C++"
        DotCover -> "dotCover"
        DotMemory -> "dotMemory"
        DotPeek -> "dotPeek"
        DotTrace -> "dotTrace"
        Rider -> "Rider"
        TeamCity -> "TeamCity"
        YouTrack -> "YouTrack"
        Upsource -> "Upsource"
        Hub -> "Hub"
        Kotlin -> "Kotlin"
        MPS -> "MPS"


decode : String -> Result String Product
decode id =
    case id of
        "jetbrains" -> Ok JetBrains
        "intellij-idea" -> Ok IntelliJ
        "phpstorm" -> Ok PhpStorm
        "pycharm" -> Ok PyCharm
        "rubymine" -> Ok RubyMine
        "webstorm" -> Ok WebStorm
        "clion" -> Ok CLion
        "datagrip" -> Ok DataGrip
        "appcode" -> Ok AppCode
        "goland" -> Ok GoLand
        "resharper" -> Ok ReSharper
        "resharper-cpp" -> Ok ReSharperCpp
        "dotcover" -> Ok DotCover
        "dotmemory" -> Ok DotMemory
        "dotpeek" -> Ok DotPeek
        "dottrace" -> Ok DotTrace
        "rider" -> Ok Rider
        "teamcity" -> Ok TeamCity
        "youtrack" -> Ok YouTrack
        "upsource" -> Ok Upsource
        "hub" -> Ok Hub
        "kotlin" -> Ok Kotlin
        "mps" -> Ok MPS
        _ -> Err id


encode : Product -> String
encode product =
    case product of
        JetBrains -> "jetbrains"
        IntelliJ -> "intellij-idea"
        PhpStorm -> "phpstorm"
        PyCharm -> "pycharm"
        RubyMine -> "rubymine"
        WebStorm -> "webstorm"
        CLion -> "clion"
        DataGrip -> "datagrip"
        AppCode -> "appcode"
        GoLand -> "goland"
        ReSharper -> "resharper"
        ReSharperCpp -> "resharper-cpp"
        DotCover -> "dotcover"
        DotMemory -> "dotmemory"
        DotPeek -> "dotpeek"
        DotTrace -> "dottrace"
        Rider -> "rider"
        TeamCity -> "teamcity"
        YouTrack -> "youtrack"
        Upsource -> "upsource"
        Hub -> "hub"
        Kotlin -> "kotlin"
        MPS -> "mps"


getLogoPath : Product -> String
getLogoPath product =
    let fileName = encode product
    in fileName ++ ".svg"


getTextLinePath : Product -> String
getTextLinePath product =
    let fileName = encode product
    in fileName ++ "-text.svg"


getCoverTextSize : Product -> ( Int, Int )
getCoverTextSize product =
    case product of
        JetBrains -> ( 90, 90 )
        IntelliJ -> ( 616, 90 )
        PhpStorm -> ( 518, 108 )
        PyCharm ->  ( 479, 108 )
        RubyMine -> ( 502, 108 )
        WebStorm -> ( 567, 90 )
        CLion -> ( 299, 90 )
        DataGrip -> ( 468, 108 )
        AppCode -> ( 518, 108 )
        GoLand -> ( 419, 90 )
        ReSharper -> ( 546, 108 )
        ReSharperCpp -> ( 763, 108 )
        DotCover -> ( 490, 90 )
        DotMemory -> ( 620, 108 )
        DotPeek -> ( 444, 90 )
        DotTrace -> ( 461, 90 )
        Rider -> ( 273, 90 )
        TeamCity -> ( 495, 108 )
        YouTrack -> ( 485, 90 )
        Upsource -> ( 490, 104 )
        Hub -> ( 211, 90 )
        Kotlin -> ( 323, 99 )
        MPS -> ( 200, 77 )


getId : Product -> ProductId
getId product =
    allProducts
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\(index, otherProduct) foundAt ->
                if (foundAt >= 0) then foundAt
                else
                    if (product == otherProduct)
                        then index
                        else -1
            )
            -1


twoLetterCode : Product -> String
twoLetterCode product =
    case product of
        JetBrains -> "JETBRAINS_"
        IntelliJ -> "IJ_"
        PhpStorm -> "PS_"
        PyCharm -> "PC_"
        RubyMine -> "RM_"
        WebStorm -> "WS_"
        CLion -> "CL_"
        DataGrip -> "DG_"
        AppCode -> "AC_"
        GoLand -> "GO_"
        ReSharper -> "R#_"
        ReSharperCpp -> "R++_"
        DotCover -> "DC_"
        DotMemory -> "DM_"
        DotPeek -> "DP_"
        DotTrace -> "DT_"
        Rider -> "RD_"
        TeamCity -> "TC_"
        YouTrack -> "YT_"
        Upsource -> "UP_"
        Hub -> "HB_"
        Kotlin -> "KT_"
        MPS -> "MPS_"


getPalette : Product -> Palette
getPalette product =
    let p = Palette in
    case product of
        JetBrains -> p "#ad3259" "#aa48ff" "#ffdb00"
        IntelliJ ->  p "#0b67cc" "#fc31fe" "#ffd08d"
        PhpStorm ->  p "#b345f1" "#765af8" "#ff318c"
        PyCharm ->   p "#09f58f" "#ed8b00" "#ffe400"
        RubyMine ->  p "#e52763" "#8f41cd" "#ea7211"
        WebStorm ->  p "#00cdd7" "#2086d7" "#fff045"
        CLion ->     p "#22d88f" "#029de0" "#ed358c"
        DataGrip ->  p "#22d88f" "#9775f8" "#ff59e6"
        AppCode ->   p "#247ce6" "#00daf0" "#1ddf93"
        GoLand ->    p "#0670c7" "#ea4fff" "#3bea62"
        ReSharper -> p "#c21456" "#e14ce3" "#fdbc2c"
        ReSharperCpp -> p "#fdbc2c" "#e14ce3" "#c21456"
        DotCover ->  p "#ff7500" "#7866ff" "#e343e6"
        DotMemory -> p "#ffbd00" "#7866ff" "#e343e6"
        DotPeek ->   p "#00caff" "#7866ff" "#e343e6"
        DotTrace ->  p "#fc1681" "#786bfb" "#e14ce3"
        Rider ->     p "#c90f5e" "#077cfb" "#fdb60d"
        TeamCity ->  p "#0cb0f2" "#905cfb" "#3bea62"
        YouTrack ->  p "#0cb0f2" "#905cfb" "#ff318c"
        Upsource ->  p "#22b1ef" "#9062f7" "#fd8224"
        Hub ->       p "#00b8f1" "#9758fb" "#ffee45"
        Kotlin ->    p "#22b1ef" "#9062f7" "#fd8224"
        MPS ->       p "#0b8fff" "#21d789" "#ffdc52"


getPaletteColor : ColorId -> Palette -> Color
getPaletteColor colorId (Palette color1 color2 color3) =
    case colorId of
        ColorI   -> color1
        ColorII  -> color2
        ColorIII -> color3


applyPalette
    :  Palette
    -> Gradient
    -> Generic.Gradient
applyPalette palette gradient =
    { stops =
        gradient.stops |>
            List.map
                (Tuple.mapSecond <| \c -> getPaletteColor c palette)
    , orientation = gradient.orientation
    }


-- TODO: convert just stops, not the whole gradient
fromGenericGradient
    :  Palette
    -> Generic.Gradient
    -> Gradient
fromGenericGradient (Palette c1 c2 c3) { stops, orientation } =
    let
        loadColor colorStr =
            if (colorStr == c1) then ColorI
            else if (colorStr == c2) then ColorII
            else if (colorStr == c3) then ColorIII
            else ColorI
    in
        { stops =
            stops |> List.map (Tuple.mapSecond loadColor)
        , orientation = orientation
        }


paletteToList : Palette -> List Color
paletteToList (Palette c1 c2 c3) =
    [ c1, c2, c3 ]


-- paletteToArray : Palette -> Array Color
-- paletteToArray = paletteToList >> Array.fromList


decodePalette : List String -> Palette
decodePalette colors =
    case colors of
        c1::c2::c3:: _ -> Palette c1 c2 c3
        _ -> transparentPalette


encodePalette : Palette -> List Color
encodePalette = paletteToList


emptyGradient : Gradient
emptyGradient =
    { stops = []
    , orientation = Gradient.Horizontal
    }


decodeGradient : Palette -> D.Decoder Gradient
decodeGradient palette =
    GenericGradient.decode
        |> D.map (fromGenericGradient palette)


encodeGradient : Palette -> Gradient -> E.Value
encodeGradient palette gradient =
    gradient
        |> applyPalette palette
        |> GenericGradient.encode
