module Style exposing (styledButton)

import Element exposing (Element)
import Element.Border
import Element.Input
import Element.Background as Background


styledButton : { onPress : Maybe msg, label : Element msg } -> Element msg
styledButton =
    Element.Input.button
        [ Background.color gray, Element.padding 10, Element.Border.width 1, Element.Border.solid, Element.Border.color <| Element.rgb 0 0 0 ]

gray : Element.Color
gray =
    Element.rgb255 241 241 241