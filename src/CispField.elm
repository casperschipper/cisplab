module CispField exposing
    ( Model
    , Msg(..)
    , addPlaceCursorEvent
    , allowedSymbols
    , arrayToString
    , blur
    , cursor
    , filter
    , init
    , insertAtList
    , subscriptions
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
import Keyboard exposing (Key(..), KeyChange(..))
import Keyboard.Arrows as Arrows


type alias Model =
    { pressedKeys : List Keyboard.Key
    , field : Array Char
    , cursorIndex : Int
    }


type Msg
    = KeyboardMessage Keyboard.Msg
    | ClickedElement Int
    | Pressed


blur : Model -> Model
blur model =
    { model | pressedKeys = [] }


init : Model
init =
    { pressedKeys = []
    , field = Array.fromList <| String.toList <| " "
    , cursorIndex = 0
    }


allowedSymbols : Char -> Bool
allowedSymbols c =
    let
        allowed =
            String.toList "()+-*/_ "
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


handleArrows : List Keyboard.Key -> Model -> Model
handleArrows arrows model =
    case arrows of
        [ Keyboard.ArrowLeft ] ->
            { model | cursorIndex = max (model.cursorIndex - 1) 0 }

        [ Keyboard.ArrowRight ] ->
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


update : Msg -> Model -> (Model, Maybe String)
update msg model =
    case msg of
        KeyboardMessage keyMsg ->
            let
                withTrail =
                    { model | field = addTrailingSpace model.field }

                ( pressedKeys, keyChange ) =
                    Keyboard.updateWithKeyChange Keyboard.anyKeyOriginal keyMsg model.pressedKeys

                newModel =
                    handleArrows pressedKeys { withTrail | pressedKeys = pressedKeys }

                controlPressed =
                    List.member Keyboard.Control pressedKeys
            in
            case ( controlPressed, keyChange ) of
                ( True, Just (KeyUp (Character "e")) ) ->
                    (gotoEndOfLine newModel , Nothing)

                ( True, Just (KeyUp (Character "a")) ) ->
                    (gotoStartOfLine newModel, Nothing)

                ( True, Just (KeyUp (Character "d")) ) ->
                    (deleteCurrentChar newModel, Nothing)

                ( True, _ ) ->
                    (newModel, Nothing)

                ( False, Just (KeyDown Backspace) ) ->
                    (deleteChar newModel, Nothing)

                ( False, Just (KeyDown (Character any)) ) ->
                    case filter any of
                        Just c ->
                            ({ newModel
                                | field = insertAt newModel.cursorIndex c newModel.field
                                , cursorIndex = newModel.cursorIndex + 1
                            }, Nothing)

                        Nothing ->
                            (newModel, Nothing)

                ( False, Just (KeyDown Keyboard.Spacebar) ) ->
                    ({ newModel
                        | field = insertAt newModel.cursorIndex ' ' newModel.field
                        , cursorIndex = newModel.cursorIndex + 1
                    }, Nothing)

                ( False, Just (KeyUp other) ) ->
                    (newModel, Nothing)

                ( False, Just (KeyDown kd) ) ->
                    (newModel,Nothing)

                ( False, Nothing ) ->
                    (newModel, Nothing)

        ClickedElement idx ->
            ({ model | cursorIndex = idx }, Nothing)

        Pressed ->
            (model, model.field |> arrayToString |> Cisp.mString)


cursor : Element.Element msg
cursor =
    Element.el [ Element.Font.color (Element.rgb 1.0 0.0 0.0) ] (Element.text "|")


arrayToString : Array Char -> String
arrayToString arr =
    arr |> Array.toList |> String.fromList


addPlaceCursorEvent : List (Element Msg) -> List (Element Msg)
addPlaceCursorEvent arr =
    let
        f idx elm =
            Element.el [ Element.Events.onClick (ClickedElement idx) ] elm
    in
    List.indexedMap f arr


insertAtList : Int -> Element msg -> List (Element msg) -> List (Element msg)
insertAtList idx a lst =
    lst |> Array.fromList |> Array.Extra.insertAt idx a |> Array.toList


markCursor : Int -> Int -> Cisp.HighlightedChars -> Element msg
markCursor idx n char =
    if n == idx then
        Cisp.toElem [ Element.Background.color (rgb 0.3 0.3 0.3) ] char

    else
        Cisp.toElem [] char


view : (Msg -> msg) -> Model -> Element msg
view toMsg model =
    let
        field =
            model.field
                |> arrayToString
                |> Cisp.colorize
                -- should result in zip with placed highlight of current cursot
                |> List.indexedMap (markCursor model.cursorIndex)
                |> addPlaceCursorEvent
                |> Element.wrappedRow [ Element.width <| Element.fillPortion 7 ,Element.Font.family [ Element.Font.monospace ] ]
    in
    Element.map toMsg
        (Element.row [ Element.width Element.fill ]
            [ field
            , Element.Input.button [Element.width <| Element.fillPortion 1 ]
                { onPress = Just Pressed
                , label = Element.text "eval"
                }
            ]
        )


subscriptions : Sub Msg
subscriptions =
    Sub.map KeyboardMessage Keyboard.subscriptions
