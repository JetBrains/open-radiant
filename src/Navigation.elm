module Navigation exposing
    ( applyUrl
    , prepareUrlFragment
    , onUrlChange
    , onUrlRequest
    )


import Browser
import Browser.Navigation exposing (..)
import Url exposing (..)


import Model.AppMode exposing (..)
import Model.SizeRule exposing (..)
import Model.Core exposing (..)


-- URL examples:
-- http://localhost:8080/#custom|100:501
-- http://localhost:8080/#preset|TW
-- http://localhost:8080/#preset|PC:2
-- http://localhost:8080/#preset|BA:200:300
-- http://localhost:8080/#viewport|1020:300
-- http://localhost:8080/#dimensionless
-- http://localhost:8080    
-- http://localhost:8080/#release/preset|TW
-- http://localhost:8080/#player
-- http://localhost:8080/#tron-dev/preset|TW



type alias Fragment = String
-- type ModeFragment = ModeFragment String
-- type SizeRuleFragment = SizeRuleFragment
--     { ruleStr: String
--     , size: Maybe ( Int, Int )
--     , factor: Maybe Int
--     }


type FragmentData
    = NoData
    | Mode AppMode
    | SizeRule SizeRule
    | ModeAndSizeRule AppMode SizeRule


applyFragment : Fragment -> Model -> Model
applyFragment fragmentStr model =
    decodeFragment fragmentStr
        |> Result.map
            (\fragment ->
                case fragment of
                    ModeAndSizeRule mode rule ->
                        { model
                        | size = rule
                        , mode = mode
                        }
                    SizeRule rule ->
                        { model
                        | size = rule
                        }
                    Mode mode ->
                        { model
                        | mode = mode
                        }
                    NoData -> model
            )
        |> Result.withDefault model


fragmentToMessage : Fragment -> Msg
fragmentToMessage fragmentStr =
    case (decodeFragment fragmentStr)
        |> Result.map
            (\fragment ->
                case fragment of
                    ModeAndSizeRule mode rule -> ChangeModeAndResize mode rule
                    SizeRule rule -> Resize rule
                    Mode mode -> ChangeMode mode
                    NoData -> NoOp
            ) of
        Ok fragmentMsg ->  fragmentMsg
        Err errMsg -> AddError <| "Unknown URL fragment: " ++ errMsg


applyUrl : Url -> Model -> Model
applyUrl url model =
    case url.fragment of
        Just fragment -> model |> applyFragment fragment
        Nothing -> model


prepareUrlFragment : Model -> Fragment
prepareUrlFragment model =
    ModeAndSizeRule model.mode model.size
        |> encodeFragment


onUrlChange : Url -> Msg
onUrlChange url =
    case url.fragment of
        Just fragment -> fragmentToMessage fragment
        Nothing -> NoOp


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest req =
    let
        _ = req
    in NoOp


decodeFragment : String -> Result String FragmentData
decodeFragment str =
    case String.split "/" str of
        modeStr::ruleStr::_ ->
            Result.map2
                ModeAndSizeRule
                (decodeMode modeStr)
                (decodeSizeRule ruleStr)
        modeOrRule::_ ->
            case decodeMode modeOrRule of
                Ok mode ->
                    Ok <| Mode mode
                Err sizeRuleStr ->
                    decodeSizeRule sizeRuleStr
                        |> Result.map SizeRule
        _ -> Ok NoData


encodeFragment : FragmentData -> String
encodeFragment data =
    case data of
        ModeAndSizeRule mode rule ->
            encodeMode mode ++ "/"
                ++ encodeSizeRule rule
        Mode mode ->
            encodeMode mode
        SizeRule rule ->
            encodeSizeRule rule
        NoData -> ""
