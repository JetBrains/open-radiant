module Model.AppMode exposing
    ( AppMode(..)
    , encodeMode
    , decodeMode
    )


type AppMode
    = Development
    | Production
    | Release
    | Ads
    | TronUi AppMode
    | Player


encodeMode : AppMode -> String
encodeMode mode =
    case mode of
        Development -> "dev"
        Production -> "prod"
        Release -> "release"
        Ads -> "ads"
        TronUi innerMode -> "tron-" ++ encodeMode innerMode
        Player -> "player"


decodeMode : String -> Result String AppMode
decodeMode mode =
    if String.startsWith "tron-" mode
    then
        String.dropLeft 5 mode
            |> decodeMode
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
