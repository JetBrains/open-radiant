module Model.Error exposing
    ( Error
    , Errors(..)
    )


type alias Error = String
type Errors = Errors (List Error) -- TODO: List (List String) ??
