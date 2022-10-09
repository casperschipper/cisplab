module Cisp exposing (..)
import Parser exposing (Parser)


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
