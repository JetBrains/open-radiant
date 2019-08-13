module Model.AppMode exposing
    ( AppMode(..)
    , default
    , encode
    , decode
    )


type AppMode
    = Development
    | Production
    | Release
    | Ads
    | TronUi AppMode
    | Player


default : AppMode
default = Release


encode : AppMode -> String
encode mode =
    case mode of
        Development -> "dev"
        Production -> "prod"
        Release -> "release"
        Ads -> "ads"
        TronUi innerMode -> "tron-" ++ encode innerMode
        Player -> "player"


decode : String -> Result String AppMode
decode mode =
    if String.startsWith "tron-" mode
    then
        String.dropLeft 5 mode
            |> decode
            |> Result.map TronUi
    else
        case mode of
            "dev" -> Ok Development
            "prod" -> Ok Production
            "release" -> Ok Release
            "ads" -> Ok Ads
            "tron" -> Ok <| TronUi Production
            "player" -> Ok Player
            _ -> Err mode
