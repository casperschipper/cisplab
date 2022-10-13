module CispField exposing (..)

import Element exposing (Element)
import Keyboard
import Parser 
import Cisp 
import Element


type alias Model =
    { keyState : List Keyboard.Key
    , field : String
    }


type Msg
    = KeyUp Keyboard.RawKey
    | KeyDown Keyboard.RawKey


init : Model
init =
    { keyState = []
    , field = ""
    }


allowedSymbols : Char -> Bool
allowedSymbols c =
    let
        allowed =
            String.toList "()+-*/_ "
    in
    List.member c allowed 


filter : String -> String
filter str =
    let
        singleChar s =
            case String.length s of
                1 ->
                    Just s

                _ ->
                    Nothing

        alphnum s =
            case String.toList s of
                [] ->
                    Nothing

                c :: _ ->
                    if Char.isAlphaNum c || allowedSymbols c then
                        Just (String.fromChar c)

                    else
                        Nothing
    in
    str
        |> singleChar
        |> Maybe.andThen alphnum
        |> Maybe.withDefault ""


update : Msg -> Model -> Model
update msg model =
    case msg of
        KeyDown raw ->
            case Keyboard.rawValue raw of
                "Backspace" ->
                    { model
                        | field =
                            String.dropRight 1 model.field
                    }

                any ->
                    { model | field = model.field ++ filter any }

        KeyUp raw ->
            let
                _ =
                    Debug.log "fish" (Keyboard.rawValue raw)
            in
            model



view : Model -> Element Msg
view model =
    Cisp.colorize model.field


subscriptions =
    Sub.batch
        [ Keyboard.ups KeyUp
        , Keyboard.downs KeyDown
        ]

