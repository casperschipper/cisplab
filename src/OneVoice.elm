module OneVoice exposing (..)

import Cisp exposing (CispProgram, cispAsString)
import Element exposing (Element, column, fill, px, width)
import Element.Input as Input
import Parameter exposing (Parameter(..))


type alias OneVoice =
    { pitch : CispProgram
    , velo : CispProgram
    , channel : CispProgram
    , duration : CispProgram
    }


init : OneVoice
init =
    { pitch = Cisp.ofString "(st 60)"
    , velo = Cisp.ofString "(st 100)"
    , channel = Cisp.ofString "(st 1)"
    , duration = Cisp.ofString "(st 0.1)"
    }


type Msg
    = Change Parameter String
    | Set Parameter


type Action
    = Update Parameter String


setParameter p m =
    let
        str =
            m.pitch |> cispAsString
    in
    Update Pitch str


changeParameter p s m =
    case p of
        Pitch ->
            { m | pitch = Cisp.ofString s }

        Velo ->
            { m | velo = Cisp.ofString s }

        Duration ->
            { m | duration = Cisp.ofString s }

        Channel ->
            { m | channel = Cisp.ofString s }


update : Msg -> OneVoice -> ( OneVoice, Maybe Action )
update msg m =
    case msg of
        Set p ->
            ( m, Just (setParameter p m) )

        Change p s ->
            ( changeParameter p s m, Nothing )


parView : Parameter -> CispProgram -> Element Msg
parView p c =
    let
        str =
            cispAsString c
    in
    Element.row [ width fill ]
        [ Input.text [ width fill ]
            { onChange = Change p
            , text = cispAsString c
            , label = Input.labelAbove [] (Element.text (Parameter.toString p))
            , placeholder = Nothing
            }
        , Input.button [ width (px 100) ]
            { onPress = Just (Set p)
            , label = Element.text "set!"
            }
        ]


view : OneVoice -> Element Msg
view model =
    column [ width fill ]
        [ parView Pitch model.pitch
        , parView Velo model.velo
        , parView Duration model.duration
        , parView Channel model.channel
        ]
