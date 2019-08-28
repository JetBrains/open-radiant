module Navigation exposing
    ( applyUrl
    , loadUrl
    , onUrlChange
    , onUrlRequest
    , pushUrlFrom
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


defaultUncertain : UncertainFragmentValue
defaultUncertain =
    UncertainFragmentValue Nothing Nothing Nothing


-- fill uncertain fragment with the values from the given model
defaultTo : Model -> UncertainFragmentValue -> FragmentValue
defaultTo model (UncertainFragmentValue maybeMode maybeProduct maybeSize) =
    FragmentValue
        (maybeMode |> Maybe.withDefault model.mode)
        (maybeProduct |> Maybe.withDefault model.product)
        (maybeSize |> Maybe.withDefault model.size)


fillDefaults : UncertainFragmentValue -> FragmentValue
fillDefaults (UncertainFragmentValue maybeMode maybeProduct maybeSize) =
    FragmentValue
        (maybeMode |> Maybe.withDefault Mode.default)
        (maybeProduct |> Maybe.withDefault Product.default)
        (maybeSize |> Maybe.withDefault SizeRule.default)


-- take current model and command to update the URL of the browser with its state
pushUrlFrom : Model -> Cmd Msg
pushUrlFrom model =
    pushUrl model.navKey <| loadUrl model


-- take the new URL (not the one stored in the model) & model,
-- and build the messages required to be performed to update it to the new state,
-- URL has the preference over the current model
applyUrl : Url -> Model -> List Msg
applyUrl url curModel =
    case url.fragment of
        Just fragment ->
            case fragment |> decodeFragment |> fillDefaults of
                FragmentValue mode product size ->
                    [ if mode /= curModel.mode then ChangeMode mode else NoOp
                    , if product /= curModel.product then ChangeProduct product else NoOp
                    , if size /= curModel.size then Resize size else NoOp
                    ] |> List.filter ((==) NoOp)
        Nothing ->
            []


-- prepareUrlFragment : Model -> Fragment
-- prepareUrlFragment model =
--     FragmentValue model.mode model.product model.size
--         |> encodeFragment


-- take current URL and model, and create the new URL representing
-- the state where the model has the preference over current URL,
-- and the defaults values are excluded
loadUrl : Model -> Fragment
loadUrl curModel =
    let
        curFragment =
            curModel.url
                |> Maybe.andThen .fragment
                |> Maybe.map decodeFragment
                |> Maybe.withDefault defaultUncertain
        injectValue fromModel fromUrl defaultValue =
            case fromUrl of
                Just urlValue ->
                    if fromModel == urlValue
                    then Just urlValue
                    else
                        if fromModel /= defaultValue
                            then Just fromModel
                            else Nothing
                Nothing ->
                    if fromModel /= defaultValue
                    then Just fromModel
                    else Nothing
    in
        case curFragment of
            UncertainFragmentValue maybeMode maybeProduct maybeSize ->
                UncertainFragmentValue
                        (injectValue curModel.mode maybeMode Mode.default)
                        (injectValue curModel.product maybeProduct Product.default)
                        (injectValue curModel.size maybeSize SizeRule.default)
                |> encodeUncertainFragment


-- there are no links, so no URL requests for the moment
-- See https://package.elm-lang.org/packages/elm/browser/latest/Browser#UrlRequest
onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest req =
    let
        _ = req
    in NoOp


onUrlChange : Url -> Msg
onUrlChange = ApplyUrl


tryIfError : String -> (String -> Result () a) -> Result () a -> Result () a
tryIfError str decoder prevVal =
    case prevVal of
        Err _ -> decoder str
        Ok v -> Ok v


-- take the list of strings and try to find values
-- among them using all the three given decoders
-- in any combination
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


-- take the URL string and collect the values if they are
-- specified in it
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


-- take uncertain fragment value and encode it into the URL,
-- skipping the Nothing values
encodeUncertainFragment : UncertainFragmentValue -> Fragment
encodeUncertainFragment uncertainFragment =
    case uncertainFragment of
        UncertainFragmentValue maybeMode maybeProduct maybeSize
            ->
                (maybeMode
                    |> Maybe.map Mode.encode
                    |> Maybe.map ((++) "/")
                    |> Maybe.withDefault "") ++
                (maybeProduct
                    |> Maybe.map Product.encode
                    |> Maybe.map ((++) "/")
                    |> Maybe.withDefault "") ++
                (maybeSize
                    |> Maybe.map SizeRule.encode
                    |> Maybe.withDefault "")


-- take current fragment value and encode it into the URL,
-- omitting the default values
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
