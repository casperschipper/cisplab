module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html)


type Model
    = Model String


type Msg
    = NoOp


init _ =
    ( Model "(seq 1 2 3)", Cmd.none )


main : Platform.Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Document Msg
view (Model str) =
    let
        body =
            Html.text str
    in
        { title = "cisp-lab"
        , body = [body]
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
