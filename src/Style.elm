module Style exposing (styledButton)

import Element exposing (Element)
import Element.Border
import Element.Input


styledButton : { onPress : Maybe msg, label : Element msg } -> Element msg
styledButton =
    Element.Input.button
        [ Element.padding 10, Element.Border.width 1, Element.Border.solid, Element.Border.color <| Element.rgb 0 0 0 ]
