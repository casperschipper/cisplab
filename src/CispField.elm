module CispField exposing
    ( Model
    , Msg(..)
    , OutMsg(..)
    , applyKeyboard
    , init
    , update
    , view
    )

import Array exposing (Array)
import Array.Extra exposing (insertAt)
import Cisp
import Element exposing (Element, rgb)
import Element.Background
import Element.Events
import Element.Font
import Element.Input
import Html exposing (input)
import Keyboard exposing (Key(..), KeyChange(..))


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


handleArrows : Maybe Keyboard.KeyChange -> Model -> Model
handleArrows marrow model =
    case marrow of
        Just (Keyboard.KeyDown Keyboard.ArrowLeft) ->
            let
                _ =
                    Debug.log "arrow left" model.cursorIndex
            in
            { model | cursorIndex = max (model.cursorIndex - 1) 0 }

        Just (Keyboard.KeyDown Keyboard.ArrowRight) ->
            let
                _ =
                    Debug.log "arrow left" model.cursorIndex
            in
            { model | cursorIndex = min (model.cursorIndex + 1) (Array.length model.field) }

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


applyKeyboard : List Keyboard.Key -> Maybe Keyboard.KeyChange -> Model -> Model
applyKeyboard pressedKeys keyChange model =
    let
        withTrail =
            { model | field = addTrailingSpace model.field }

        newModel =
            handleArrows keyChange withTrail

        controlPressed =
            List.member Keyboard.Control pressedKeys
    in
    case ( controlPressed, keyChange ) of
        ( True, Just (KeyUp (Character "e")) ) ->
            gotoEndOfLine newModel

        ( True, Just (KeyUp (Character "a")) ) ->
            gotoStartOfLine newModel

        ( True, Just (KeyUp (Character "d")) ) ->
            deleteCurrentChar newModel

        ( True, _ ) ->
            newModel

        ( False, Just (KeyDown Backspace) ) ->
            deleteChar newModel

        ( False, Just (KeyDown (Character any)) ) ->
            case filter any of
                Just c ->
                    { newModel
                        | field = insertAt newModel.cursorIndex c newModel.field
                        , cursorIndex = newModel.cursorIndex + 1
                    }

                Nothing ->
                    newModel

        ( False, Just (KeyDown Keyboard.Spacebar) ) ->
            { newModel
                | field = insertAt newModel.cursorIndex ' ' newModel.field
                , cursorIndex = newModel.cursorIndex + 1
            }

        ( False, Just (KeyUp _) ) ->
            newModel

        ( False, Just (KeyDown _) ) ->
            newModel

        ( False, Nothing ) ->
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
        Cisp.toElem [ Element.Background.color (rgb 0.3 0.3 0.3) ] char

    else
        Cisp.toElem [] char


markCursor : Bool -> Int -> List Cisp.HighlightedChars -> List (Element msg)
markCursor active n array =
    if active then
        List.indexedMap (markCursorAt n) array

    else
        List.map (\char -> Cisp.toElem [] char) array


bypass : Bool -> (a -> a) -> a -> a
bypass shouldBypass f input =
    if shouldBypass then
        f input

    else
        input


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
    Element.row [ Element.width Element.fill ]
        [ field
        , Element.Input.button [ Element.width <| Element.fillPortion 1 ]
            { onPress = Just Eval
            , label = Element.text "eval"
            }
        ]
