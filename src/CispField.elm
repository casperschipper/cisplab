module CispField exposing (..)

import Keyboard
import Element exposing (Element)


type alias Model =
    { keyState : List Keyboard.Key
    , field : String
    }


type Msg
    = KeyUp Keyboard.RawKey
    | KeyDown Keyboard.RawKey


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
                    { model | field = model.field ++ any }

        KeyUp raw ->
            let
                _ =
                    Debug.log "fish" (Keyboard.rawValue raw)
            in
            model

view : Model -> Element Msg 
view model =
    Element.text (model.field)

subscriptions =
    Sub.batch
        [ Keyboard.ups KeyUp
        , Keyboard.downs KeyDown
        ]
