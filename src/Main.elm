port module Main exposing (Action(..), Model, Msg(..), OneVoice, SelectedCisp(..), black, blurs, buttonStyle, display, getCispField, handleAction, init, initVoice, input, main, parView, receiveSocketMsg, sendSocketCommand, subscriptions, update, updatePar, view, viewChar, viewVoice, white, wssend)

import Array exposing (Array)
import Browser exposing (Document)
import Cisp exposing (..)
import CispField
import Element exposing (Element, column, fill, px, width)
import Element.Background
import Element.Border
import Element.Font
import Element.Input as Input
import Html exposing (Html)
import Html.Events exposing (custom)
import Json.Decode as JD
import Json.Encode as JE
import Keyboard
import Parameter exposing (Parameter(..))
import Parser exposing (Parser)
import WebSocket exposing (WebSocketCmd(..))


port receiveSocketMsg : (JD.Value -> msg) -> Sub msg


port sendSocketCommand : JE.Value -> Cmd msg


port blurs : (() -> msg) -> Sub msg


type SelectedCisp
    = SelectedCisp Int Parameter


type alias Model =
    { cisp : CispProgram
    , custom : String
    , cisps : Array OneVoice
    , selected : SelectedCisp
    }


type Msg
    = CispString String
    | ReceivedFrame (Result JD.Error WebSocket.WebSocketMsg)
    | SendMessage String
    | SetCustom String
    | SendCustom
    | Blur
    | KeyboardMsg Keyboard.Msg
    | CispFieldMsg Int Parameter CispField.Msg


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
    { cisp = Invalid ""
    , custom = ""
    , cisps = Array.fromList [ initVoice ]
    , selected = SelectedCisp 0 Pitch
    }
        |> update (CispString input)
        |> Tuple.mapSecond (\_ -> websocketCmd)


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


buttonStyle : List (Element.Attribute msg)
buttonStyle =
    [ Element.centerX
    , Element.Border.solid
    , Element.Border.width 1
    , Element.Border.rounded 5
    , Element.padding 10
    ]


display : Model -> Html Msg
display { cisp, custom, cisps } =
    let
        textInputStyle =
            [ Element.centerX, Element.width (Element.px 1000) ]

        cispsView =
            Element.column [ Element.width Element.fill ]
                (cisps
                    |> Array.indexedMap (\idx voice -> viewVoice idx voice)
                    |> Array.toList
                )
    in
    Element.layout
        [ Element.width Element.fill, Element.Background.color black ]
        (Element.column [ Element.centerX, Element.spacing 25 ]
            [ -- [ Element.Input.text textInputStyle
              --     { onChange = CispString
              --     , text = cispAsString cisp
              --     , placeholder = Nothing
              --     , label = Element.Input.labelAbove [ Element.Font.color white ] <| Element.text "CISP:"
              --     }
              -- , case cisp of
              --     Valid csp ->
              --         Element.Input.button buttonStyle
              --             { onPress = Just (SendMessage csp)
              --             , label = Element.text "Send to CISP"
              --             }
              --     Invalid _ ->
              --         Element.text "not valid"
              -- , Element.Input.text textInputStyle
              --     { onChange = SetCustom
              --     , text = custom
              --     , placeholder = Nothing
              --     , label = Element.Input.labelAbove [] <| Element.text "custom ws mesage"
              --     }
              -- , Element.Input.button buttonStyle { onPress = Just SendCustom, label = Element.text "send custom" }
              cispsView
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
                    ( { model | cisp = Valid str, custom = model.custom, cisps = model.cisps }, Cmd.none )

                Err _ ->
                    ( { model | cisp = Invalid str, custom = model.custom, cisps = model.cisps }, Cmd.none )

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
            case model.cisp of
                Valid str ->
                    ( model, WebSocket.Send { name = "cisp", content = "/1/pitch/ " ++ str } |> wssend )

                Invalid _ ->
                    ( model, Cmd.none )

        SetCustom str ->
            ( { model | custom = str }, Cmd.none )

        SendCustom ->
            ( model, WebSocket.Send { name = "cisp", content = model.custom } |> wssend )

        -- Send messages to websocket, a Msg that is triggered when hitting "eval"
        Blur ->
            -- TODO should blur all cispfields
            let
                _ =
                    Debug.todo "fish"
            in
            ( model, Cmd.none )

        KeyboardMsg kmsg ->
            -- map to current voice
            let
                cispFieldMsg = 
                    case model.selected of
                        SelectedCisp idx parameter ->
                            CispFieldMsg idx parameter (CispField.createKeyboardMsg kmsg)
            in
            update cispFieldMsg model

        CispFieldMsg idx parameter fieldMsg ->
            let
                mvoice =
                    Array.get idx model.cisps
            in
            case mvoice of
                Just voice ->
                    case updatePar idx parameter fieldMsg voice of
                        ( newVoice, maction ) ->
                            {model | cisps = Array.set idx newVoice model.cisps} |> handleAction maction

                Nothing ->
                    ( model, Cmd.none )


handleAction : Maybe Action -> Model -> ( Model, Cmd Msg )
handleAction action model =
    case action of
        Nothing ->
            ( model, Cmd.none )

        Just (Update idx par str) ->
            let
                idxstr =
                    String.fromInt idx

                address =
                    String.join "/" [ idxstr, Parameter.toString par ]
            in
            ( model, WebSocket.Send { name = "cisp", content = "/" ++ address ++ " " ++ str } |> wssend )

        Just (HighLight idx param) ->
            ( { model | selected = SelectedCisp idx param }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveSocketMsg <| WebSocket.receive ReceivedFrame
        , blurs (\() -> Blur)
        , Sub.map KeyboardMsg Keyboard.subscriptions
        ]


wssend : WebSocketCmd -> Cmd msg
wssend =
    WebSocket.send sendSocketCommand


getCispField : Int -> Parameter -> Array OneVoice -> Maybe CispField.Model
getCispField idx par array =
    let
        getPar p voice =
            case p of
                Pitch ->
                    voice.pitch

                Velo ->
                    voice.velo

                Duration ->
                    voice.duration

                Channel ->
                    voice.channel
    in
    Array.get idx array |> Maybe.map (getPar par)


updatePar : Int -> Parameter -> CispField.Msg -> OneVoice -> ( OneVoice, Maybe Action )
updatePar voiceIndex parameter msg voice =
    case parameter of
        Pitch ->
            let
                ( newPar, outMsg ) =
                    CispField.update msg voice.pitch

                mAction =
                    outMsg
                        |> Maybe.map
                            (\out ->
                                case out of
                                    CispField.Highlight ->
                                        HighLight voiceIndex parameter

                                    CispField.EvalString str ->
                                        Update voiceIndex parameter str
                            )
            in
            ( { voice | pitch = newPar }, mAction )

        Velo ->
            let
                ( newPar, outMsg ) =
                    CispField.update msg voice.velo

                mAction =
                    outMsg
                        |> Maybe.map
                            (\out ->
                                case out of
                                    CispField.Highlight ->
                                        HighLight voiceIndex parameter

                                    CispField.EvalString str ->
                                        Update voiceIndex parameter str
                            )
            in
            ( { voice | velo = newPar }, mAction )

        Duration ->
            let
                ( newPar, outMsg ) =
                    CispField.update msg voice.duration

                mAction =
                    outMsg
                        |> Maybe.map
                            (\out ->
                                case out of
                                    CispField.Highlight ->
                                        HighLight voiceIndex parameter

                                    CispField.EvalString str ->
                                        Update voiceIndex parameter str
                            )
            in
            ( { voice | duration = newPar }, mAction )

        Channel ->
            let
                ( newPar, outMsg ) =
                    CispField.update msg voice.duration

                mAction =
                    outMsg
                        |> Maybe.map
                            (\out ->
                                case out of
                                    CispField.Highlight ->
                                        HighLight voiceIndex parameter

                                    CispField.EvalString str ->
                                        Update voiceIndex parameter str
                            )
            in
            ( { voice | duration = newPar }, mAction )


type alias OneVoice =
    { pitch : CispField.Model
    , velo : CispField.Model
    , channel : CispField.Model
    , duration : CispField.Model
    }


initVoice : OneVoice
initVoice =
    { pitch = CispField.init "(st 60)"
    , velo = CispField.init "(st 100)"
    , channel = CispField.init "(st 1)"
    , duration = CispField.init "(st 0.1)"
    }


type Action
    = Update Int Parameter String
    | HighLight Int Parameter


parView : Int -> Parameter -> CispField.Model -> Element Msg
parView voiceNumber p cispField =
    let
        str =
            Parameter.toString p
    in
    Element.map (CispFieldMsg voiceNumber p) (CispField.view cispField)


viewVoice : Int -> OneVoice -> Element Msg
viewVoice idx voice =
    column [ width fill ]
        [ parView idx Pitch voice.pitch
        , parView idx Velo voice.velo
        , parView idx Duration voice.duration
        , parView idx Channel voice.channel
        ]
