module Cisp exposing (..)

import Parser exposing (Parser)
import Element exposing (Element)
import Element.Font

type Sexpr
    = Slist (List Sexpr)
    | Value CValue


type CValue
    = CNumber Float
    | CString String
    | CispWord String


type CispProgram
    = Invalid String
    | Valid String


ofString : String -> CispProgram
ofString s =
    case Parser.run clist s of
        Ok _ ->
            Valid s

        Err _ ->
            Invalid s


cispAsString : CispProgram -> String
cispAsString cp =
    case cp of
        Invalid s ->
            s

        Valid s ->
            s


cispwords : List String
cispwords =
    [ "seq"
    , "line"
    , "ch"
    , "rv"
    , "walk"
    , "transcat"
    , "cycle"
    , "st"
    , "index"
    ]


type alias Depth =
    Int


type Tree a
    = Node a
    | Tree Depth (List (Tree a))


type CispWord
    = Seq


cispNumber : Parser CValue
cispNumber =
    Parser.number
        { int = Just (\i -> i |> toFloat |> CNumber)
        , hex = Nothing -- 0x001A is allowed
        , octal = Nothing -- 0o0731 is not
        , binary = Nothing -- 0b1101 is not
        , float = Just CNumber
        }


parseString : String -> CValue
parseString str =
    if List.member str cispwords then
        CispWord str

    else
        CString str


value : Parser CValue
value =
    Parser.oneOf
        [ cispNumber
        , Parser.chompWhile Char.isAlpha |> Parser.getChompedString |> Parser.map parseString
        ]


clist : Parser Sexpr
clist =
    Parser.sequence
        { start = "("
        , separator = " "
        , end = ")"
        , spaces = Parser.succeed ()
        , item =
            Parser.oneOf
                [ Parser.lazy (\_ -> sexpr) -- hmm why does this one have to come first ?
                , value |> Parser.map Value
                ]
        , trailing = Parser.Forbidden
        }
        |> Parser.map Slist


sexpr : Parser Sexpr
sexpr =
    clist



makeTree : Sexpr -> Tree CValue
makeTree expr =
    let
        aux d e =
            case e of
                Slist lst ->
                    Tree d (lst |> List.map (aux (d + 1)))

                Value v ->
                    Node v
    in
    aux 0 expr

renderTree : Tree CValue -> Element.Element msg
renderTree tree =
    case tree of
        Tree n lst ->
            let
                clr =
                    color n

                chars =
                    List.map renderTree lst
                        |> List.intersperse space
                        |> (\xs -> leftColon clr :: xs ++ [ rightColon clr ])
            in
            Element.paragraph [] chars

        Node v ->
            cvalueToElement v


cvalueToElement : CValue -> Element msg
cvalueToElement cvalue =
    case cvalue of
        CNumber flt ->
            Element.text (String.fromFloat flt) |> Element.el [ Element.Font.color (Element.rgb 0.0 0.4 0.0) ]

        CString str ->
            Element.text str |> Element.el [ Element.Font.color (Element.rgb 0.0 1.0 1.0) ]

        CispWord str ->
            Element.text str |> Element.el [ Element.Font.color (Element.rgb 0.4 0.5 0.0) ]
color : Int -> Element.Color
color c =
    let
        rgb =
            Element.rgb255
    in
    case modBy 6 c of
        0 ->
            rgb 200 30 30

        1 ->
            rgb 30 200 30

        2 ->
            rgb 30 30 200

        3 ->
            rgb 50 50 100

        4 ->
            rgb 100 50 50

        5 ->
            rgb 100 100 100

        _ ->
            rgb 200 100 200
            
leftColon : Element.Color -> Element msg
leftColon cattr =
    Element.el [ Element.Font.color cattr ] <| Element.text "("


rightColon : Element.Color -> Element msg
rightColon cattr =
    Element.el [ Element.Font.color cattr ] <| Element.text ")"


space : Element msg
space =
    Element.el [] (Element.text " ")

colorize : String -> Element msg
colorize cispString =
    let
        result =
            Parser.run sexpr cispString

        _ =
            Debug.log "result:\n" result
    in
    case result of
        Ok res ->
            res |> makeTree |> renderTree

        Err _ ->
            Element.text ("~" ++ cispString)