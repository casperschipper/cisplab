port module Main exposing (CValue(..), CispWord(..), Depth, Model(..), Msg(..), Sexpr(..), Tree(..), main)

import Browser exposing (Document)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import Json.Decode as JD
import Json.Encode as JE
import Parser exposing (Parser)
import WebSocket exposing (WebSocketCmd)
import WebSocket exposing (WebSocketCmd(..))



-- TODO: introduce simple text field to issue hand written commands


port receiveSocketMsg : (JD.Value -> msg) -> Sub msg


port sendSocketCommand : JE.Value -> Cmd msg


type CispProgram
    = Invalid String
    | Valid String


cispAsString : CispProgram -> String
cispAsString cp =
    case cp of
        Invalid s ->
            s

        Valid s ->
            s


type Model
    = Model
        { cisp : CispProgram
        , custom : String
        }

getCustom : Model -> String
getCustom (Model model) = 
    model.custom

getCisp : Model -> CispProgram
getCisp (Model model) =
    model.cisp



type Msg
    = CispString String
    | ReceivedFrame (Result JD.Error WebSocket.WebSocketMsg)
    | SendMessage String
    | SendCustomString String


type Sexpr
    = Slist (List Sexpr)
    | Value CValue


type CValue
    = CNumber Float
    | CString String
    | CispWord String


cispwords : List String
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


input : String
input =
    "(seq 1 (seq 4 5) 3)"


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        websocketCmd =
            WebSocket.Connect
                { name = "cisp"
                , address = "ws://127.0.0.1:3000"
                , protocol = "json"
                }
                |> wssend
    in
    Model { cisp = Invalid "", custom = "" } |> update (CispString input) |> Tuple.mapSecond (\_ -> websocketCmd)


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


view : Model -> Document Msg
view model =
    { title = "cisp-lab"
    , body = [ display model ]
    }


colorize : String -> Element Msg
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


black : Element.Color
black =
    Element.rgb 1.0 1.0 1.0


white : Element.Color
white =
    Element.rgb 1 1 1


display : Model -> Html Msg
display (Model { cisp, custom }) =
    Element.layout
        [ Element.width Element.fill, Element.Background.color black ]
        (Element.column [ Element.centerX, Element.spacing 25 ]
            [ Element.Input.text [ Element.centerX ]
                { onChange = CispString
                , text = cispAsString cisp
                , placeholder = Nothing
                , label = Element.Input.labelAbove [ Element.Font.color white ] <| Element.text "CISP:"
                }
            , colorize (cispAsString cisp)
            , case cisp of
                Valid csp ->
                    Element.Input.button
                        [ Element.centerX
                        , Element.Border.solid
                        , Element.Border.width 1
                        , Element.Border.rounded 5
                        , Element.padding 10
                        ]
                        { onPress = Just (SendMessage csp)
                        , label = Element.text "Send to CISP"
                        }

                Invalid _ ->
                    Element.text "not valid"
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CispString str ->
            let
                result =
                    Parser.run sexpr str
            in
            case result of
                Ok _ ->
                    ( Model { cisp = (Valid str), custom = getCustom model }, Cmd.none )

                Err _ ->
                    ( Model { cisp = (Invalid str), custom = getCustom model }, Cmd.none )

        ReceivedFrame result ->
            let
                _ =
                    case result of
                        Err err ->
                            JD.errorToString err |> Debug.log "err"

                        Ok websockMsg ->
                            case websockMsg of
                                WebSocket.Error err ->
                                    let
                                        _ =
                                            Debug.log "error,string" err
                                    in
                                    ""

                                WebSocket.Data de ->
                                    let
                                        _ =
                                            Debug.log "success!" de
                                    in
                                    ""
            in
            ( model, Cmd.none )

        SendMessage _ ->
            case getCisp model of
                (Valid str) ->
                    ( model, WebSocket.Send { name = "foo", content = "/1/pitch/ " ++ str } |> wssend )

                (Invalid _) ->
                    ( model, Cmd.none )

        SendCustomString str ->
            (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveSocketMsg <| WebSocket.receive ReceivedFrame


wssend : WebSocketCmd -> Cmd msg
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


leftColon : Element.Color -> Element msg
leftColon cattr =
    Element.el [ Element.Font.color cattr ] <| Element.text "("


rightColon : Element.Color -> Element msg
rightColon cattr =
    Element.el [ Element.Font.color cattr ] <| Element.text ")"


space : Element msg
space =
    Element.el [] (Element.text " ")


cvalueToElement : CValue -> Element Msg
cvalueToElement cvalue =
    case cvalue of
        CNumber flt ->
            Element.text (String.fromFloat flt) |> Element.el [ Element.Font.color (Element.rgb 0.0 0.4 0.0) ]

        CString str ->
            Element.text str |> Element.el [ Element.Font.color (Element.rgb 0.0 1.0 1.0) ]

        CispWord str ->
            Element.text str |> Element.el [ Element.Font.color (Element.rgb 0.4 0.5 0.0) ]


renderTree : Tree CValue -> Element Msg
renderTree tree =
    case tree of
        Tree n lst ->
            let
                clr =
                    color n

                chars =
                    List.map renderTree lst
                        |> List.intersperse space
                        |> (\xs -> leftColon clr :: xs ++ [ rightColon clr ])
            in
            Element.paragraph [] chars

        Node v ->
            cvalueToElement v
