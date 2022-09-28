module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes as Attr


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


viewChar : Char -> Html Msg
viewChar c =
    Html.span [] [Html.text (String.fromChar c)]


showText : String -> Html Msg
showText str =
    let
        chars =
            String.toList str
    in
    Html.p [ Attr.style "font-family" "monospace" ] <| (List.map viewChar chars)


view : Model -> Document Msg
view (Model str) =
    { title = "cisp-lab"
    , body = [ showText str ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
