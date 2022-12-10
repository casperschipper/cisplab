module CispField exposing
    ( Model
    , Msg(..)
    , OutMsg(..)
    , applyKeyboard
    , decoder
    , encode
    , init
    , update
    , view
    )

import Array exposing (Array)
import Array.Extra exposing (insertAt)
import Cisp
import Element exposing (Element)
import Element.Background
import Element.Events
import Element.Font
import Json.Decode
import Json.Encode
import Keyboard exposing (Key(..), KeyChange(..))
import Style


type alias Model =
    { field : Array Char
    , cursorIndex : Int
    }


init : String -> Model
init initial =
    { field = Array.fromList <| String.toList <| initial
    , cursorIndex = 0
    }


allowedSymbols : Char -> Bool
allowedSymbols c =
    let
        allowed =
            String.toList "()+-*/_. "
    in
    List.member c allowed


filter : String -> Maybe Char
filter str =
    let
        singleChar s =
            case String.toList s of
                [ c ] ->
                    Just c

                _ ->
                    Nothing

        alphnum c =
            if Char.isAlphaNum c || allowedSymbols c then
                Just c

            else
                Nothing
    in
    str
        |> singleChar
        |> Maybe.andThen alphnum


gotoEndOfLine : Model -> Model
gotoEndOfLine model =
    { model
        | cursorIndex = Array.length model.field
    }


gotoStartOfLine : Model -> Model
gotoStartOfLine model =
    { model
        | cursorIndex = 0
    }


moveLeft : Model -> Model
moveLeft model =
    { model | cursorIndex = max (model.cursorIndex - 1) 0 }


moveRight : Model -> Model
moveRight model =
    { model | cursorIndex = min (model.cursorIndex + 1) (Array.length model.field) }


handleArrows : Maybe Keyboard.KeyChange -> Model -> Model
handleArrows marrow model =
    case marrow of
        Just (Keyboard.KeyDown Keyboard.ArrowLeft) ->
            moveLeft model

        Just (Keyboard.KeyDown Keyboard.ArrowRight) ->
            moveRight model

        _ ->
            model


deleteChar : Model -> Model
deleteChar model =
    let
        prev =
            max (model.cursorIndex - 1) 0
    in
    { model
        | field =
            Array.Extra.removeAt prev model.field
        , cursorIndex = prev
    }


deleteCurrentChar : Model -> Model
deleteCurrentChar model =
    { model
        | field = Array.Extra.removeAt model.cursorIndex model.field
    }


addTrailingSpace : Array Char -> Array Char
addTrailingSpace array =
    let
        size =
            Array.length array
    in
    case Array.get (size - 1) array of
        Just ' ' ->
            array

        _ ->
            Array.push ' ' array


type Msg
    = JumpToLocation Int
    | Eval


type OutMsg
    = Highlight
    | EvalString String


type CharState
    = Start
    | InWord
    | Done
    | InvalidPosition


currentChar : Model -> Maybe Char
currentChar model =
    Array.get model.cursorIndex model.field


isSpace : Char -> Bool
isSpace c =
    c == ' ' || c == '\t' || c == '\n'


deleteTail : Model -> Model
deleteTail model =
    { model | field = Array.slice 0 model.cursorIndex model.field }


gotoPreviousWord : Model -> Model
gotoPreviousWord model =
    let
        helper state mdl =
            case model.cursorIndex of
                0 ->
                    model

                other ->
                    if model.cursorIndex == Array.length model.field - 1 then
                        model

                    else
                        case state of
                            Done ->
                                mdl

                            Start ->
                                case mdl |> moveLeft |> currentChar of
                                    Just c ->
                                        if isSpace c then
                                            mdl |> helper Start

                                        else
                                            mdl |> helper InWord

                                    Nothing ->
                                        helper InvalidPosition mdl

                            InWord ->
                                case mdl |> moveLeft |> currentChar of
                                    Just c ->
                                        if isSpace c then
                                            mdl |> helper Done

                                        else
                                            mdl |> helper InWord

                                    Nothing ->
                                        helper InvalidPosition mdl

                            InvalidPosition ->
                                { mdl | cursorIndex = 0 }
    in
    helper Start model


gotoNextWord : Model -> Model
gotoNextWord model =
    let
        helper state mdl =
            if model.cursorIndex == Array.length model.field - 1 then
                model

            else
                case state of
                    Done ->
                        mdl

                    Start ->
                        case mdl |> moveRight |> currentChar of
                            Just c ->
                                if isSpace c then
                                    mdl |> helper Start

                                else
                                    mdl |> helper InWord

                            Nothing ->
                                helper InvalidPosition mdl

                    InWord ->
                        case mdl |> moveRight |> currentChar of
                            Just c ->
                                if isSpace c then
                                    mdl |> helper Done

                                else
                                    mdl |> helper InWord

                            Nothing ->
                                helper InvalidPosition mdl

                    InvalidPosition ->
                        { mdl | cursorIndex = Array.length model.field - 1 }
    in
    helper Start model


applyKeyboard : List Keyboard.Key -> Maybe Keyboard.KeyChange -> Model -> Model
applyKeyboard pressedKeys keyChange model =
    let
        withTrail =
            { model | field = addTrailingSpace model.field }

        newModel =
            handleArrows keyChange withTrail

        controlPressed =
            List.member Keyboard.Control pressedKeys

        altPressed =
            List.member Keyboard.Alt pressedKeys
    in
    case ( altPressed, controlPressed, keyChange ) of
        ( True, _, Just (KeyUp Keyboard.ArrowRight) ) ->
            gotoPreviousWord newModel

        ( True, _, Just (KeyUp Keyboard.ArrowLeft) ) ->
            gotoNextWord newModel

        ( _, True, Just (KeyUp (Character "e")) ) ->
            gotoEndOfLine newModel

        ( _, True, Just (KeyUp (Character "a")) ) ->
            gotoStartOfLine newModel

        ( _, True, Just (KeyUp (Character "d")) ) ->
            deleteCurrentChar newModel

        ( _, True, Just (KeyUp (Character "k")) ) ->
            deleteTail newModel

        ( _, True, _ ) ->
            newModel

        ( _, False, Just (KeyDown Backspace) ) ->
            deleteChar newModel

        ( _, False, Just (KeyDown (Character any)) ) ->
            case filter any of
                Just c ->
                    { newModel
                        | field = insertAt newModel.cursorIndex c newModel.field
                        , cursorIndex = newModel.cursorIndex + 1
                    }

                Nothing ->
                    newModel

        ( _, False, Just (KeyDown Keyboard.Spacebar) ) ->
            { newModel
                | field = insertAt newModel.cursorIndex ' ' newModel.field
                , cursorIndex = newModel.cursorIndex + 1
            }

        ( _, False, Just (KeyUp _) ) ->
            newModel

        ( _, False, Just (KeyDown _) ) ->
            newModel

        ( _, False, Nothing ) ->
            newModel


update : Msg -> Model -> ( Model, Maybe OutMsg )
update msg model =
    case msg of
        JumpToLocation location ->
            ( { model | cursorIndex = location }, Just Highlight )

        Eval ->
            ( model, Just (EvalString <| arrayToString model.field) )


arrayToString : Array Char -> String
arrayToString arr =
    arr |> Array.toList |> String.fromList


addPlaceCursorEvent : (Int -> Msg) -> List (Element Msg) -> List (Element Msg)
addPlaceCursorEvent clickMsg arr =
    let
        f idx elm =
            Element.el [ Element.Events.onClick (clickMsg idx) ] elm
    in
    List.indexedMap f arr


markCursorAt : Int -> Int -> Cisp.HighlightedChars -> Element msg
markCursorAt idx n char =
    if n == idx then
        Cisp.toElem [ Element.Background.color (Element.rgb 0.3 0.3 0.3) ] char

    else
        Cisp.toElem [] char


markCursor : Bool -> Int -> List Cisp.HighlightedChars -> List (Element msg)
markCursor active n array =
    if active then
        List.indexedMap (markCursorAt n) array

    else
        List.map (\char -> Cisp.toElem [] char) array


encode : Model -> Json.Encode.Value
encode model =
    Json.Encode.string (model.field |> arrayToString)


decoder : Json.Decode.Decoder Model
decoder =
    Json.Decode.string
        |> Json.Decode.map
            (\str ->
                { field = str |> String.toList |> Array.fromList
                , cursorIndex = 0
                }
            )


view : Bool -> Model -> Element Msg
view isActive model =
    let
        field =
            model.field
                |> arrayToString
                |> Cisp.colorize
                -- should result in zip with placed highlight of current cursot
                |> markCursor isActive model.cursorIndex
                |> addPlaceCursorEvent (\idx -> JumpToLocation idx)
                |> Element.wrappedRow [ Element.width <| Element.fillPortion 7, Element.Font.family [ Element.Font.monospace ] ]
    in
    Element.row [ Element.Background.color (Element.rgb 0.97 0.97 0.9), Element.width Element.fill ]
        [ field
        , Style.styledButton
            { onPress = Just Eval
            , label = Element.text "eval"
            }
        ]
