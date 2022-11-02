module Cisp exposing (..)

import Element exposing (Element)
import Element.Font
import Html exposing (b)
import Parser exposing ((|.), (|=), Parser, getChompedString)


type Depth
    = Depth Int


increase (Depth d) =
    Depth (d + 1)


type Sexpr
    = Slist (List Sexpr)
    | Value CValue
    | ParOpen Depth
    | ParClose Depth
    | Space


type CValue
    = CNumber Float
    | CString String
    | CispWord String


type CispProgram
    = Invalid String
    | Valid String


ofString : String -> CispProgram
ofString s =
    case Parser.run sexpr s of
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
    , "+"
    , "-"
    , "/"
    , "~"
    , "t"
    ]


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
        , Parser.succeed ()
            |. Parser.chompIf Char.isAlpha
            |. Parser.chompWhile Char.isAlpha -- ChompWhile is dangerous!!! 
            |> Parser.getChompedString
            |> Parser.map parseString
        ]


parseSpace : Parser (List Sexpr)
parseSpace =
    Parser.chompWhile (\c -> c == ' ' || c == '\n' || c == '\u{000D}')
        -- zero or more
        |> Parser.getChompedString
        |> Parser.map
            (\str ->
                String.length str |> (\n -> List.repeat n Space)
            )


complete : Parser Sexpr
complete =
    let
        helper depth expression_lst =
            parseSpace
                |> Parser.andThen
                    (\spaces ->
                        let
                            _ =
                                Debug.log "spaces:" spaces
                        in
                        Parser.oneOf
                            -- OK IT is important that you first do the recursive case !! but why ??
                            [ Parser.succeed identity
                                |. Parser.symbol "("
                                |= (Parser.loop [] (helper (increase depth)) |> Parser.map (\v -> Parser.Loop (v :: spaces ++ expression_lst)))
                            , Parser.symbol ")"
                                |> Parser.map (\_ -> Parser.Done (Slist (ParOpen depth :: List.reverse (ParClose depth :: spaces ++ expression_lst))))
                            , value |> Parser.map (\v -> Parser.Loop (Value v :: spaces ++ expression_lst)) -- this is the line that blew up!
                            , Parser.end
                                |> Parser.andThen (\_ -> Parser.problem "expecting closing parenthesis: )")
                            ]
                    )
    in
    -- Parser.succeed (ParClose (Depth 0))
    Parser.succeed identity
        |. Parser.symbol "("
        |= Parser.loop [] (helper (Depth 0))



-- clist : Depth -> Parser Sexpr
-- clist depth =
--     Parser.sequence
--         { start = "("
--         , separator = " "
--         , end = ")"
--         , spaces = Parser.succeed ()
--         , item =
--             Parser.oneOf
--                 [ Parser.lazy (\_ -> clist (increase depth)) -- hmm why does this one have to come first ?
--                 , value |> Parser.map Value
--                 ]
--         , trailing = Parser.Forbidden
--         }
--         |> Parser.map Slist


sexpr : Parser Sexpr
sexpr =
    complete


type HighlightedChars
    = Colored Char Element.Color
    | Uncolored Char



{-
   colorTree : Tree CValue -> Tree (Colored CValue)
   colorTree tree =
       case tree of
           Tree n [] ->
               Tree n []

           Tree n lst ->
               let clr =
                   color n

                   chars =
                        lst
                           |> List.concatMap colorTree
                           |> (\xs -> leftColon clr :: List.intersperse space xs ++ [ rightColon clr ])
               in
               chars

           Node v ->
-}


renderTree : Sexpr -> List HighlightedChars
renderTree exp =
    case exp of
        Slist [] ->
            []

        Slist lst ->
            lst |> List.concatMap renderTree

        Value v ->
            cvalueToElement v

        ParClose d ->
            [ Colored ')' (ofDepth d) ]

        ParOpen d ->
            [ Colored '(' (ofDepth d) ]

        Space ->
            [ Uncolored ' ' ]


cvalueToElement : CValue -> List HighlightedChars
cvalueToElement cvalue =
    case cvalue of
        CNumber flt ->
            flt |> String.fromFloat |> String.toList |> List.map (\c -> Colored c (Element.rgb 0.0 0.4 0.0))

        CString str ->
            str |> String.toList |> List.map (\c -> Colored c (Element.rgb 0.4 0.4 0.0))

        CispWord str ->
            str |> String.toList |> List.map (\c -> Colored c (Element.rgb 0.4 0.5 0.0))


ofDepth : Depth -> Element.Color
ofDepth (Depth c) =
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


leftColon : Element.Color -> HighlightedChars
leftColon =
    Colored '('


rightColon : Element.Color -> HighlightedChars
rightColon =
    Colored ')'


space : HighlightedChars
space =
    Uncolored ' '


colorize : String -> List HighlightedChars
colorize cispString =
    let
        result =
            Parser.run sexpr cispString

        _ =
            Debug.log "result:\n" result
    in
    case result of
        Ok res ->
            res |> renderTree

        Err _ ->
            cispString |> String.toList |> List.map (\c -> Uncolored c)


toElem : HighlightedChars -> Element msg
toElem c =
    case c of
        Colored char color ->
            Element.el [ Element.Font.color color ] (Element.text (String.fromChar char))

        Uncolored char ->
            Element.el [] (Element.text (String.fromChar char))
