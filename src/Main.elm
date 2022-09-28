module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes as Attr
import Parser exposing ((|.), (|=), Parser, number)


type Model
    = Model String


type Msg
    = NoOp


type Sexpr
    = Slist (List Sexpr)
    | Value CValue


type CValue
    = CNumber Float
    | CString String


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
    let
        _ =
            Debug.log input (Parser.run sexpr input)
    in
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
