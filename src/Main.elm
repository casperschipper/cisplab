port module Main exposing (Action(..), Model, Msg(..), OneVoice, SelectedCisp(..), main)

import Array exposing (Array)
import Browser exposing (Document)
import Cisp exposing (..)
import CispField
import Element exposing (Element, column, fill, width)
import Element.Background
import Html exposing (Html)
import Json.Decode as JD
import Json.Encode as JE
import Keyboard
import Parameter exposing (Parameter(..))
import Parser
import WebSocket exposing (WebSocketCmd)


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
    | Blur
    | KeyboardMsg Keyboard.Msg
    | CispFieldMsg Int Parameter CispField.Msg


input : String
input =
    "(seq 1 (seq 4 5) 3)"


init : flags -> ( Model, Cmd Msg )
init _ =
    { cisp = Invalid ""
    , custom = ""
    , cisps = Array.fromList [ initVoice ]
    , selected = SelectedCisp 0 Pitch
    }
        |> update (CispString input)
        |> Tuple.mapSecond
            (\_ ->
                WebSocket.Connect
                    { name = "cisp"
                    , address = "ws://127.0.0.1:3000"
                    , protocol = "json"
                    }
                    |> wssend
            )


main : Platform.Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Document Msg
view model =
    { title = "cisp-lab"
    , body = [ display model ]
    }


black : Element.Color
black =
    Element.rgb 1.0 1.0 1.0


display : Model -> Html Msg
display { cisp, custom, cisps } =
    let
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
                    ( { model | cisp = Valid str }, Cmd.none )

                Err _ ->
                    ( { model | cisp = Invalid str }, Cmd.none )

        ReceivedFrame _ ->
            ( model, Cmd.none )

        -- Send messages to websocket, a Msg that is triggered when hitting "eval"
        Blur ->
            -- TODO should blur all cispfields
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
                    let
                        ( newVoice, maction ) =
                            updatePar idx parameter fieldMsg voice
                    in
                    { model | cisps = Array.set idx newVoice model.cisps } |> handleAction maction

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
                    CispField.update msg voice.channel

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
            ( { voice | channel = newPar }, mAction )


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
    Element.map (CispFieldMsg voiceNumber p) (CispField.view cispField)


viewVoice : Int -> OneVoice -> Element Msg
viewVoice idx voice =
    column [ width fill ]
        [ parView idx Pitch voice.pitch
        , parView idx Velo voice.velo
        , parView idx Duration voice.duration
        , parView idx Channel voice.channel
        ]
