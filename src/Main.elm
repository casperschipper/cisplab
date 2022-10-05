port module Main exposing (..)

import Browser exposing (Document)
import Element exposing (Element)
import Element.Background
import Element.Font
import Element.Input
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as JD
import Json.Encode as JE
import Parser exposing ((|.), (|=), Parser)
import WebSocket exposing (WebSocketCmd)


port receiveSocketMsg : (JD.Value -> msg) -> Sub msg


port sendSocketCommand : JE.Value -> Cmd msg


type Model
    = Model String


type Msg
    = CispString String
    | ReceivedFrame (Result JD.Error WebSocket.WebSocketMsg)
    | SendMessage String


type Sexpr
    = Slist (List Sexpr)
    | Value CValue


type CValue
    = CNumber Float
    | CString String
    | CispWord String


cvalueToString v =
    case v of
        CNumber flt ->
            String.fromFloat flt

        CString str ->
            str

        CispWord str ->
            str


cispwords =
    [ "seq"
    , "line"
    , "ch"
    , "rv"
    , "walk"
    , "transcat"
    , "cycle"
    , "st"
    , "index"
    ]


toString : Sexpr -> String
toString sexp =
    case sexp of
        Slist lst ->
            "(" ++ String.join " " (List.map toString lst) ++ ")"

        Value v ->
            cvalueToString v


type alias Depth =
    Int


type Tree a
    = Node a
    | Tree Depth (List (Tree a))


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


parseString : String -> CValue
parseString str =
    if List.member str cispwords then
        CispWord str

    else
        CString str


value : Parser CValue
value =
    Parser.oneOf
        [ cispNumber
        , Parser.chompWhile Char.isAlpha |> Parser.getChompedString |> Parser.map parseString
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
        websocketCmd =
            WebSocket.Connect
                { name = "foo"
                , address = "ws://127.0.0.1:3000"
                , protocol = "json"
                }
                |> wssend

        batch =
            Cmd.batch [ websocketCmd ]
    in
    ( Model input, batch )


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
view model =
    { title = "cisp-lab"
    , body = [ display model ]
    }


colorize cispString =
    let
        result =
            Parser.run sexpr cispString

        _ =
            Debug.log "result:\n" result
    in
    case result of
        Ok res ->
            res |> makeTree |> renderTree

        Err _ ->
            Element.text "oh no!"


black =
    Element.rgb 0.4 0.1 0.1


white =
    Element.rgb 1 1 1


display (Model current) =
    Element.layout
        [ Element.width Element.fill, Element.Background.color black ]
        (Element.column [ Element.centerX, Element.spacing 25 ]
            [ Element.Input.button []
                { onPress = Just (SendMessage "/list")
                , label = Element.text "push me!!!"
                }
            , Element.Input.text []
                { onChange = CispString
                , text = current
                , placeholder = Nothing
                , label = Element.Input.labelAbove [ Element.Font.color white ] <| Element.text "CISP:"
                }
            , colorize current
            ]
        )



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CispString str ->
            ( Model str, Cmd.none )

        ReceivedFrame result ->
            let 
                _ = 
                    case result of 
                        Err err -> (JD.errorToString err) |> Debug.log "err"

                        Ok websockMsg -> 
                            case websockMsg of
                                WebSocket.Error err -> let _ = Debug.log "error,string" err in ""

                                WebSocket.Data de -> 
                                    let _ = Debug.log "success!" de in ""

            in
            case model of
                Model str ->
                    ( Model str, Cmd.none )

        SendMessage message ->
            ( model, WebSocket.Send { name = "foo", content = "/list" } |> wssend )


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveSocketMsg <| WebSocket.receive ReceivedFrame


wssend =
    WebSocket.send sendSocketCommand


makeTree : Sexpr -> Tree CValue
makeTree expr =
    let
        aux d e =
            case e of
                Slist lst ->
                    Tree d (lst |> List.map (aux (d + 1)))

                Value v ->
                    Node v
    in
    aux 0 expr


color : Int -> Element.Color
color c =
    let
        rgb =
            Element.rgb255
    in
    case modBy 6 c of
        0 ->
            rgb 200 30 30

        1 ->
            rgb 30 200 30

        2 ->
            rgb 30 30 200

        3 ->
            rgb 50 50 100

        4 ->
            rgb 100 50 50

        5 ->
            rgb 100 100 100

        _ ->
            rgb 200 100 200


leftColon cattr =
    Element.el [ Element.Font.color cattr ] <| Element.text "("


rightColon cattr =
    Element.el [ Element.Font.color cattr ] <| Element.text ")"


space =
    Element.el [] (Element.text " ")


cvalueToElement : CValue -> Element Msg
cvalueToElement cvalue =
    case cvalue of
        CNumber flt ->
            Element.text (String.fromFloat flt) |> Element.el [ Element.Font.color (Element.rgb 0.8 0.8 1.0) ]

        CString str ->
            Element.text str |> Element.el [ Element.Font.color (Element.rgb 0.0 1.0 1.0) ]

        CispWord str ->
            Element.text str |> Element.el [ Element.Font.color (Element.rgb 1.0 1.0 0.0) ]


renderTree tree =
    case tree of
        Tree n lst ->
            let
                clr =
                    color n

                chars =
                    List.map renderTree lst
                        |> List.intersperse space
                        |> (\xs -> [ leftColon clr ] ++ xs ++ [ rightColon clr ])
            in
            Element.paragraph [] chars

        Node v ->
            cvalueToElement v
