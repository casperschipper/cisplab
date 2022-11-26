port module Main exposing (Action(..), Model, Msg(..), OneVoice, SelectedCisp(..), main)

import Array exposing (Array)
import Browser exposing (Document)
import Cisp exposing (..)
import CispField
import Element exposing (Element, column, fill, width)
import Element.Background
import Element.Input exposing (OptionState(..))
import Element.Region exposing (..)
import Html exposing (Html)
import Json.Decode as JD
import Json.Encode as JE
import Keyboard
import Parameter exposing (Parameter(..))
import Parser
import WebSocket exposing (WebSocketCmd)


port receiveSocketMsg : (JD.Value -> msg) -> Sub msg


port sendSocketCommand : JE.Value -> Cmd msg


port blurs : (Int -> msg) -> Sub msg


type SelectedCisp
    = SelectedCisp Int Parameter


nextParameter : Parameter -> Parameter
nextParameter p =
    case p of
        Pitch ->
            Velo

        Velo ->
            Duration

        Duration ->
            Channel

        Channel ->
            Pitch


previousParameter : Parameter -> Parameter
previousParameter p =
    case p of
        Pitch ->
            Channel

        Channel ->
            Duration

        Duration ->
            Velo

        Velo ->
            Pitch


nextVoice : Model -> SelectedCisp
nextVoice model =
    let
        length =
            Array.length model.cisps
    in
    case model.selected of
        SelectedCisp idx par ->
            if idx == (length - 1) then
                SelectedCisp 0 par

            else
                SelectedCisp (idx + 1) par


previousVoice : Model -> SelectedCisp
previousVoice model =
    let
        length =
            Array.length model.cisps
    in
    case model.selected of
        SelectedCisp idx par ->
            if idx == 0 then
                SelectedCisp (length - 1) par

            else
                SelectedCisp (idx - 1) par


selectedNextParameter : SelectedCisp -> SelectedCisp
selectedNextParameter (SelectedCisp n p) =
    SelectedCisp n (nextParameter p)


selectedPreviousParameter : SelectedCisp -> SelectedCisp
selectedPreviousParameter (SelectedCisp n p) =
    SelectedCisp n (previousParameter p)


isActive : Int -> Parameter -> SelectedCisp -> Bool
isActive voice par (SelectedCisp i p) =
    (voice == i) && (par == p)


type alias KeyboardState =
    { keys : List Keyboard.Key
    , change : Maybe Keyboard.KeyChange
    }


type alias Model =
    { cisp : CispProgram
    , custom : String
    , cisps : Array OneVoice
    , selected : SelectedCisp
    , keyboard : KeyboardState
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
    { cisp = Valid ""
    , custom = ""
    , cisps = Array.fromList [ initVoice, initVoice ]
    , selected = SelectedCisp 0 Pitch
    , keyboard = { keys = [], change = Nothing }
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
display model =
    let
        cispsView =
            Element.column [ Element.width Element.fill ]
                (Element.text "use shift and arrows to switch between voices"
                    :: (model.cisps
                            |> Array.indexedMap (\idx voice -> viewVoice idx voice model.selected)
                            |> Array.toList
                       )
                )
    in
    Element.layout
        [ Element.width Element.fill, Element.Background.color black ]
        (Element.column [ Element.centerX, Element.spacing 25 ]
            [ cispsView
            ]
        )


handleArrows : Maybe Keyboard.KeyChange -> Model -> Model
handleArrows change model =
    let
        controlDown =
            List.member Keyboard.Shift model.keyboard.keys
    in
    if controlDown then
        case change of
            Just (Keyboard.KeyDown Keyboard.ArrowDown) ->
                { model
                    | selected = nextVoice model
                }

            Just (Keyboard.KeyDown Keyboard.ArrowUp) ->
                { model
                    | selected = previousVoice model
                }

            _ ->
                model

    else
        case change of
            Just (Keyboard.KeyDown Keyboard.ArrowDown) ->
                { model | selected = selectedNextParameter model.selected }

            Just (Keyboard.KeyDown Keyboard.ArrowUp) ->
                { model | selected = selectedPreviousParameter model.selected }

            _ ->
                model


updateArrayAtIndex : Int -> (a -> a) -> Array a -> Result String (Array a)
updateArrayAtIndex idx fupdate array =
    case Array.get idx array of
        Just val ->
            Ok (Array.set idx (fupdate val) array)

        Nothing ->
            Err "This index does not exist"


updateSelectedCisp : (CispField.Model -> CispField.Model) -> Model -> Model
updateSelectedCisp updateFun model =
    let
        selectedCisp =
            model.selected

        updatePr : Parameter -> OneVoice -> OneVoice
        updatePr para voice =
            case para of
                Pitch ->
                    { voice | pitch = updateFun voice.pitch }

                Velo ->
                    { voice | velo = updateFun voice.velo }

                Duration ->
                    { voice | duration = updateFun voice.duration }

                Channel ->
                    { voice | channel = updateFun voice.channel }
    in
    case model.selected of
        SelectedCisp idx par ->
            case updateArrayAtIndex idx (updatePr par) model.cisps of
                Ok newCisps ->
                    { model | cisps = newCisps }

                Err err ->
                    let
                        _ =
                            Debug.log "udpateSelectedCisp" err
                    in
                    model


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
                ( newKeys, change ) =
                    Keyboard.updateWithKeyChange Keyboard.anyKeyOriginal kmsg model.keyboard.keys

                newModel =
                    { model | keyboard = { keys = newKeys, change = change } }

                newModel2 =
                    newModel |> handleArrows change
            in
            ( newModel2 |> updateSelectedCisp (CispField.applyKeyboard newKeys change), Cmd.none )

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
        , blurs (\_ -> Blur)
        , Sub.map KeyboardMsg Keyboard.subscriptions
        ]


wssend : WebSocketCmd -> Cmd msg
wssend =
    WebSocket.send sendSocketCommand


mkAction : Int -> Parameter -> Maybe CispField.OutMsg -> Maybe Action
mkAction voiceIndex parameter outMsg =
    outMsg
        |> Maybe.map
            (\out ->
                case out of
                    CispField.Highlight ->
                        HighLight voiceIndex parameter

                    CispField.EvalString str ->
                        Update voiceIndex parameter str
            )


updatePar : Int -> Parameter -> CispField.Msg -> OneVoice -> ( OneVoice, Maybe Action )
updatePar voiceIndex parameter msg voice =
    case parameter of
        Pitch ->
            let
                ( newPar, outMsg ) =
                    CispField.update msg voice.pitch
            in
            ( { voice | pitch = newPar }, mkAction voiceIndex parameter outMsg )

        Velo ->
            let
                ( newPar, outMsg ) =
                    CispField.update msg voice.velo
            in
            ( { voice | velo = newPar }, mkAction voiceIndex parameter outMsg )

        Duration ->
            let
                ( newPar, outMsg ) =
                    CispField.update msg voice.duration
            in
            ( { voice | duration = newPar }, mkAction voiceIndex parameter outMsg )

        Channel ->
            let
                ( newPar, outMsg ) =
                    CispField.update msg voice.channel
            in
            ( { voice | channel = newPar }, mkAction voiceIndex parameter outMsg )


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


parView : Int -> Parameter -> SelectedCisp -> CispField.Model -> Element Msg
parView voiceNumber p selectedCisp cispField =
    Element.map (CispFieldMsg voiceNumber p) (CispField.view (isActive voiceNumber p selectedCisp) cispField)


edges =
    { top = 0, left = 0, right = 0, bottom = 0 }


viewVoice : Int -> OneVoice -> SelectedCisp -> Element Msg
viewVoice idx voice selectedCisp =
    column [ width (Element.px 1024), Element.spacingXY 40 20 ]
        [ Element.el
            [ Element.Region.heading 1
            , Element.paddingEach { edges | top = 50 }
            ]
          <|
            Element.text ("Voice: " ++ String.fromInt (idx + 1))
        , parView idx Pitch selectedCisp voice.pitch
        , parView idx Velo selectedCisp voice.velo
        , parView idx Duration selectedCisp voice.duration
        , parView idx Channel selectedCisp voice.channel
        ]
