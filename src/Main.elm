module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Parser exposing ((|.), (|=), Parser, number)


type Model
    = Model String


type Msg
    = CispString String


type Sexpr
    = Slist (List Sexpr)
    | Value CValue


type CValue
    = CNumber Float
    | CString String


toString : Sexpr -> String
toString sexp =
    case sexp of
        Slist lst ->
            "(" ++ String.join " " (List.map toString lst) ++ ")"

        Value v ->
            case v of
                CNumber flt ->
                    String.fromFloat flt

                CString str ->
                    str


type CispWord
    = Seq


cispNumber : Parser CValue
cispNumber =
    Parser.number
        { int = Just (\i -> i |> toFloat |> CNumber)
        , hex = Nothing -- 0x001A is allowed
        , octal = Nothing -- 0o0731 is not
        , binary = Nothing -- 0b1101 is not
        , float = Just CNumber
        }


value : Parser CValue
value =
    Parser.oneOf
        [ cispNumber
        , Parser.chompWhile Char.isAlpha |> Parser.getChompedString |> Parser.map CString
        ]


clist : Parser Sexpr
clist =
    Parser.sequence
        { start = "("
        , separator = " "
        , end = ")"
        , spaces = Parser.succeed ()
        , item =
            Parser.oneOf
                [ Parser.lazy (\_ -> sexpr) -- hmm why does this one have to come first ?
                , value |> Parser.map Value
                ]
        , trailing = Parser.Forbidden
        }
        |> Parser.map Slist


sexpr : Parser Sexpr
sexpr =
    clist


parse str =
    Parser.run sexpr str


input =
    "(seq 1 (seq 4 5) 3)"


init _ =
    ( Model input, Cmd.none )


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
    Html.span [] [ Html.text (String.fromChar c) ]


showText : String -> Html Msg
showText str =
    let
        chars =
            String.toList str
    in
    Html.p [ Attr.style "font-family" "monospace" ] <| List.map viewChar chars


textInput : Html Msg
textInput =
    Html.input [ Html.Events.onInput CispString ] []


view : Model -> Document Msg
view (Model str) =
    { title = "cisp-lab"
    , body = [ textInput, showText str ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CispString str ->
            let
                result =
                    Parser.run sexpr str

                result_str =
                    case result of
                        Ok res ->
                            res |> toString

                        Err _ ->
                            "Sorry it ididn't work out"
            in
            ( Model result_str, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
