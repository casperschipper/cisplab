module OneVoice exposing (..)

import Cisp exposing ( cispAsString, sexpr)
import Element exposing (Element, column, fill, px, width)
import Element.Input as Input
import Parameter exposing (Parameter(..))
import CispField 


type alias OneVoice =
    { pitch : CispField.Model
    , velo : CispField.Model
    , channel : CispField.Model
    , duration : CispField.Model
    }


init : OneVoice
init =
    { pitch = CispField.init "(st 60)"
    , velo = CispField.init "(st 100)"
    , channel = CispField.init "(st 1)"
    , duration = CispField.init "(st 0.1)"
    }


type Msg
    = Change Parameter CispField.Msg


type Action
    = Update Parameter String





update : Msg -> OneVoice -> ( OneVoice, Maybe Action )
update msg m =
    case msg of
        Change p cspMsg ->
            let 
                (newP,mString) =
                    CispField.update cspMsg m.pitch 
            in
            ({ m | pitch = newP },mString |> Maybe.map (\str -> Update p str))



parView : Parameter -> CispField.Model -> Element Msg
parView p c =
    let 
        str = 
            Parameter.toString p 
    in
    CispField.view (Change p) c
    


view : OneVoice -> Element Msg
view model =
    column [ width fill ]
        [ parView Pitch model.pitch
        , parView Velo model.velo
        , parView Duration model.duration
        , parView Channel model.channel
        ]
