module Model.Layer.Def exposing (..)

import Gui.Def exposing (Nest)

import Json.Decode as D
import Json.Encode as E

import Model.Layer.Blend.WebGL as WebGL

import Model.Layer.Context exposing (Context)
import Model.Layer.Broadcast as Broadcast exposing (Msg)


type alias DefId = String

type Index = Index Int
type alias JsIndex = Int -- index for ports


type Kind
    = Html
    | WebGL
    | Canvas
    | JS


type alias Def model view msg blend =
    { id : DefId
    , kind : Kind
    , init : Index -> Context -> ( model, Cmd msg )
    , encode : Context -> model -> E.Value
    , decode : Context -> D.Decoder model
    , update : Index -> Context -> msg -> model -> ( model, Cmd msg )
    , response : Index -> Context -> Broadcast.Msg -> model -> ( model, Cmd msg )
    , view : Index -> Context -> Maybe blend -> model -> view
    , subscribe : Context -> model -> Sub ( Index, msg )
    , gui : Maybe (Index -> model -> Nest msg)
    }


unit : Def () () () ()
unit =
    { id = "unit"
    , kind = JS
    , init = initWith ()
    , encode = noEncode
    , decode = decodeTo ()
    , update = passUpdate
    , response = passResponse
    , view = singleView ()
    , subscribe = noSubscriptions
    , gui = Nothing
    }


passUpdate : Index -> Context -> msg -> model -> ( model, Cmd msg )
passUpdate = \_ _ _ model -> ( model, Cmd.none )


passResponse : Index -> Context -> Broadcast.Msg -> model -> ( model, Cmd msg )
passResponse = \_ _ _ model -> ( model, Cmd.none )


singleView : view -> Index -> Context -> Maybe blend -> model -> view
singleView v = \_ _ _ _ -> v


initWith : model -> Index -> Context -> ( model, Cmd msg )
initWith m = \_ _ -> ( m, Cmd.none )


noEncode : Context -> model -> E.Value
noEncode = \_ _ -> E.object []


decodeTo : model -> Context -> D.Decoder model
decodeTo v = \_ -> D.succeed v


noSubscriptions : Context -> model -> Sub ( Index, msg )
noSubscriptions = \_ _ -> Sub.none


indexToString : Index -> String
indexToString (Index index) = String.fromInt index


empty : DefId -> Kind -> model -> view -> Def model view () ()
empty id kind initialModel initalView =
    { id = id
    , kind = kind
    , init = initWith initialModel
    , encode = noEncode
    , decode = decodeTo initialModel
    , subscribe = noSubscriptions
    , update = passUpdate
    , response = passResponse
    , view = singleView initalView
    , gui = Nothing
    }
