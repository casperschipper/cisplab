port module Main exposing (Model(..), Msg(..), black, buttonStyle, display, getCisp, getCispField, getCisps, getCustom, handleAction, init, input, main, receiveSocketMsg, sendSocketCommand, setCisps, subscriptions, update, view, viewChar, white, wssend)

import Array exposing (Array)
import Browser exposing (Document)
import Cisp exposing (..)
import CispField
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import Html.Events exposing (custom)
import Json.Decode as JD
import Json.Encode as JE
import OneVoice exposing (Action(..), OneVoice)
import Parameter exposing (Parameter)
import Parser exposing (Parser)
import WebSocket exposing (WebSocketCmd(..))


port receiveSocketMsg : (JD.Value -> msg) -> Sub msg


port sendSocketCommand : JE.Value -> Cmd msg


type Model
    = Model
        { cisp : CispProgram
        , custom : String
        , cisps : Array OneVoice
        , cispField : CispField.Model
        }


getCustom : Model -> String
getCustom (Model model) =
    model.custom


getCisp : Model -> CispProgram
getCisp (Model model) =
    model.cisp


getCisps : Model -> Array OneVoice
getCisps (Model m) =
    m.cisps


setCisps : Array OneVoice -> Model -> Model
setCisps cisps (Model m) =
    Model { m | cisps = cisps }


getCispField : Model -> CispField.Model
getCispField (Model model) =
    model.cispField


type Msg
    = CispString String
    | ReceivedFrame (Result JD.Error WebSocket.WebSocketMsg)
    | SendMessage String
    | SetCustom String
    | SendCustom
    | VoiceMsg Int OneVoice.Msg
    | CispFieldMsg CispField.Msg


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
    Model { cisp = Invalid "", custom = "", cisps = Array.fromList [ OneVoice.init ], cispField = CispField.init } |> update (CispString input) |> Tuple.mapSecond (\_ -> websocketCmd)


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


black : Element.Color
black =
    Element.rgb 1.0 1.0 1.0


white : Element.Color
white =
    Element.rgb 1 1 1


buttonStyle =
    [ Element.centerX
    , Element.Border.solid
    , Element.Border.width 1
    , Element.Border.rounded 5
    , Element.padding 10
    ]


display : Model -> Html Msg
display (Model { cisp, custom, cisps, cispField }) =
    let
        textInputStyle =
            [ Element.centerX, Element.width (Element.px 1000) ]

        cispsView =
            Element.column [ Element.width Element.fill ]
                (cisps
                    |> Array.indexedMap (\idx voice -> Element.map (\v -> VoiceMsg idx v) (OneVoice.view voice))
                    |> Array.toList
                )
    in
    Element.layout
        [ Element.width Element.fill, Element.Background.color black ]
        (Element.column [ Element.centerX, Element.spacing 25 ]
            [ Element.Input.text textInputStyle
                { onChange = CispString
                , text = cispAsString cisp
                , placeholder = Nothing
                , label = Element.Input.labelAbove [ Element.Font.color white ] <| Element.text "CISP:"
                }
            , Cisp.colorize (cispAsString cisp)
            , case cisp of
                Valid csp ->
                    Element.Input.button buttonStyle
                        { onPress = Just (SendMessage csp)
                        , label = Element.text "Send to CISP"
                        }

                Invalid _ ->
                    Element.text "not valid"
            , Element.Input.text textInputStyle
                { onChange = SetCustom
                , text = custom
                , placeholder = Nothing
                , label = Element.Input.labelAbove [] <| Element.text "custom ws mesage"
                }
            , Element.Input.button buttonStyle { onPress = Just SendCustom, label = Element.text "send custom" }
            , cispsView
            , CispField.view CispFieldMsg cispField
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
                    ( Model { cisp = Valid str, custom = getCustom model, cisps = getCisps model, cispField = getCispField model }, Cmd.none )

                Err _ ->
                    ( Model { cisp = Invalid str, custom = getCustom model, cisps = getCisps model, cispField = getCispField model }, Cmd.none )

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
                Valid str ->
                    ( model, WebSocket.Send { name = "cisp", content = "/1/pitch/ " ++ str } |> wssend )

                Invalid _ ->
                    ( model, Cmd.none )

        SetCustom str ->
            case model of
                Model m ->
                    ( Model { m | custom = str }, Cmd.none )

        SendCustom ->
            ( model, WebSocket.Send { name = "cisp", content = getCustom model } |> wssend )

        VoiceMsg idx vmsg ->
            let
                mVoiceAction =
                    Array.get idx (getCisps model)
                        |> Maybe.map (OneVoice.update vmsg)

                ( updatedCisps, cmd ) =
                    case mVoiceAction of
                        Just ( v, act ) ->
                            ( Array.set idx v (getCisps model), handleAction idx act )

                        Nothing ->
                            ( getCisps model, Cmd.none )
            in
            ( setCisps updatedCisps model, cmd )

        CispFieldMsg cmsg ->
            case model of
                Model m ->
                    ( Model { m | cispField = CispField.update cmsg (getCispField model) }, Cmd.none )


handleAction : Int -> Maybe Action -> Cmd Msg
handleAction n action =
    case action of
        Nothing ->
            Cmd.none

        Just (Update par str) ->
            let
                istr =
                    String.fromInt n

                address =
                    String.join "/" [ istr, Parameter.toString par ]
            in
            WebSocket.Send { name = "cisp", content = "/" ++ address ++ " " ++ str } |> wssend


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveSocketMsg <| WebSocket.receive ReceivedFrame
        , Sub.map CispFieldMsg CispField.subscriptions
        ]


wssend : WebSocketCmd -> Cmd msg
wssend =
    WebSocket.send sendSocketCommand
 