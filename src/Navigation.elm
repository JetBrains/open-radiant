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
import Model.AppMode as Mode exposing (decode, default)
import Model.SizeRule exposing (..)
import Model.SizeRule as SizeRule exposing (decode, default)
import Model.Product exposing (..)
import Model.Product as Product exposing (decode, default)
import Model.Core exposing (..)


-- URL examples:
-- http://localhost:8080
-- http://localhost:8080/#100x501
-- http://localhost:8080/#custom:100x501
-- http://localhost:8080/#preset:TW
-- http://localhost:8080/#preset:PCx2
-- http://localhost:8080/#preset:BA:200x300
-- http://localhost:8080/#viewport:1020x300
-- http://localhost:8080/#dimensionless
-- http://localhost:8080/#dev/custom:100x501
-- http://localhost:8080/#release/preset:TW
-- http://localhost:8080/#dev
-- http://localhost:8080/#player
-- http://localhost:8080/#tron-dev/preset:TW
-- http://localhost:8080/#jetbrains/100x501
-- http://localhost:8080/#jetbrains/dev/custom:100x501



type alias Fragment = String
-- type ModeFragment = ModeFragment String
-- type SizeRuleFragment = SizeRuleFragment
--     { ruleStr: String
--     , size: Maybe ( Int, Int )
--     , factor: Maybe Int
--     }


type FragmentValue
    = FragmentValue AppMode Product SizeRule

type UncertainFragmentValue
    = UncertainFragmentValue (Maybe AppMode) (Maybe Product) (Maybe SizeRule)


default : FragmentValue
default =
    FragmentValue Mode.default Product.default SizeRule.default


defaultTo : Model -> UncertainFragmentValue -> FragmentValue
defaultTo model (UncertainFragmentValue maybeMode maybeProduct maybeSize) =
    FragmentValue
        (maybeMode |> Maybe.withDefault model.mode)
        (maybeProduct |> Maybe.withDefault model.product)
        (maybeSize |> Maybe.withDefault model.size)


applyFragment : Fragment -> Model -> Model
applyFragment fragment curModel =
    case fragment |> decodeFragment |> defaultTo curModel  of
        FragmentValue mode product size ->
            { curModel
            | size = size
            , mode = mode
            , product = product
            }


fragmentToMessage : Fragment -> Msg
fragmentToMessage fragment =
    case decodeFragment fragment of
        UncertainFragmentValue maybeMode maybeProduct maybeSize ->
            ApplyUrl maybeMode maybeProduct maybeSize


applyUrl : Url -> Model -> Model
applyUrl url model =
    case url.fragment of
        Just fragment -> model |> applyFragment fragment
        Nothing -> model


prepareUrlFragment : Model -> Fragment
prepareUrlFragment model =
    FragmentValue model.mode model.product model.size
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


tryIfError : String -> (String -> Result () a) -> Result () a -> Result () a
tryIfError str decoder prevVal =
    case prevVal of
        Err _ -> decoder str
        Ok v -> Ok v


tryDecode3
     : List String
    -> (String -> Result () a)
    -> (String -> Result () b)
    -> (String -> Result () c)
    -> (Result () a -> Result () b -> Result () c -> d)
    -> d
tryDecode3
    strings decoder1 decoder2 decoder3 f =
    let
        foldingF str ( first, second, third ) =
            ( first  |> tryIfError str decoder1
            , second |> tryIfError str decoder2
            , third  |> tryIfError str decoder3
            )
        results =
            List.foldl
                foldingF
                ( Err (), Err (), Err () )
                strings
    in
        case results of
            ( first, second, third )
                -> f first second third


decodeFragment : Fragment -> UncertainFragmentValue
decodeFragment str =
    tryDecode3
        (String.split "/" str)
        (Mode.decode >> Result.mapError (always ()))
        (Product.decode >> Result.mapError (always ()))
        (SizeRule.decode >> Result.mapError (always ()))
        (\m p s ->
            UncertainFragmentValue
                (Result.toMaybe m)
                (Result.toMaybe p)
                (Result.toMaybe s))


encodeFragment : FragmentValue -> Fragment
encodeFragment current =
    case (default, current) of
        ( FragmentValue defaultMode defaultProduct defaultSize, FragmentValue currentMode currentProduct currentSize ) ->
                    (if defaultMode == currentMode
                        then "" else Mode.encode currentMode ++ "/") ++
                    (if defaultProduct == currentProduct
                        then "" else Product.encode currentProduct ++ "/") ++
                    (if defaultSize == currentSize
                        then "" else SizeRule.encode currentSize)
