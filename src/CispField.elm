module CispField exposing (..)

import Array exposing (Array)
import Cisp
import Element exposing (Element)
import Keyboard


type alias Model =
    { keyState : List Keyboard.Key
    , field : Array Char
    , cursorIndex : Int
    }


type Msg
    = KeyUp Keyboard.RawKey
    | KeyDown Keyboard.RawKey


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


arrayToString : Array Char -> String
arrayToString arr =
    arr |> Array.toList |> String.fromList


view : (Msg -> msg) -> Model -> Element msg
view toMsg model =
    Element.paragraph [] (Cisp.colorize (model.field |> arrayToString))


subscriptions =
    Sub.batch
        [ Keyboard.ups KeyUp
        , Keyboard.downs KeyDown
        ]
