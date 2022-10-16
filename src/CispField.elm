module CispField exposing (..)

import Array exposing (Array)
import Array.Extra exposing (insertAt)
import Cisp
import Element exposing (Element)
import Element.Events
import Element.Font
import Keyboard


type alias Model =
    { keyState : List Keyboard.Key
    , field : Array Char
    , cursorIndex : Int
    }


type Msg
    = KeyUp Keyboard.RawKey
    | KeyDown Keyboard.RawKey
    | ClickedElement Int


init : Model
init =
    { keyState = []
    , field = Array.empty
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        KeyDown raw ->
            case Keyboard.rawValue raw of
                "Backspace" ->
                    { model
                        | field =
                            Array.slice 0 (Array.length model.field - 1) model.field
                    }

                any ->
                    { model
                        | field =
                            case filter any of
                                Just c ->
                                    Array.push c model.field

                                Nothing ->
                                    model.field
                    }

        KeyUp raw ->
            let
                _ =
                    Debug.log "fish" (Keyboard.rawValue raw)
            in
            model

        ClickedElement idx ->
            { model | cursorIndex = idx }


cursor : Element.Element msg
cursor =
    Element.el [ Element.Font.color (Element.rgb 1.0 0.0 0.0) ] (Element.text "|")


arrayToString : Array Char -> String
arrayToString arr =
    arr |> Array.toList |> String.fromList


addPlaceCursorEvent arr =
    let
        f idx elm =
            Element.el [ Element.Events.onClick (ClickedElement idx) ] elm
    in
    List.indexedMap f arr


insertAtList idx a lst =
    lst |> Array.fromList |> Array.Extra.insertAt idx a |> Array.toList


view : (Msg -> msg) -> Model -> Element msg
view toMsg model =
    Element.map toMsg
        (Element.paragraph [] (Cisp.colorize (model.field |> arrayToString) |> addPlaceCursorEvent |> insertAtList model.cursorIndex cursor))


subscriptions =
    Sub.batch
        [ Keyboard.ups KeyUp
        , Keyboard.downs KeyDown
        ]
