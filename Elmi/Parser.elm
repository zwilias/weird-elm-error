module Elmi.Parser exposing (..)

{-|


# Set of helpers to parse lists of HEX


## Types

@docs Hex, Parser


# Functions

@docs take, andThen, (|=), with, (|.), parseInt, parseString, parseList, parseListHelp, parseUnion, parseTuple, parseMaybe, parseBool

-}

import Bitwise
import Elmi.Ascii
import Result


{-| -}
type alias Hex =
    Int


intLength : Int
intLength =
    8


{-| -}
type alias Tape =
    List Hex


{-| -}
type Parser a
    = Parser (Tape -> Result String ( a, Tape ))


{-| -}
succeed : a -> Parser a
succeed val =
    Parser <|
        \tape ->
            Ok ( val, tape )


fail : String -> Parser a
fail error =
    Parser <| \_ -> Err error


{-| -}
map : (a -> b) -> Parser a -> Parser b
map fn (Parser parserFn) =
    Parser <|
        \tape ->
            parserFn tape |> Result.map (Tuple.mapFirst fn)


{-| -}
apply : Parser (a -> b) -> Parser a -> Parser b
apply aToB next =
    aToB |> andThen (\fn -> map fn next)


{-| -}
(|=) : Parser (a -> b) -> Parser a -> Parser b
(|=) =
    apply


{-| -}
andThen : (a -> Parser b) -> Parser a -> Parser b
andThen toParserB (Parser parserAFn) =
    Parser <|
        \tape ->
            case parserAFn tape of
                Ok ( value, tape_ ) ->
                    let
                        (Parser parserBFn) =
                            toParserB value
                    in
                    parserBFn tape_

                Err err ->
                    Err err


read : Parser Int
read =
    Parser <|
        \tape ->
            case tape of
                x :: xs ->
                    Ok ( x, xs )

                [] ->
                    Err "End of tape"


take : Int -> Parser (List Int)
take count =
    Parser <|
        \tape ->
            let
                taken : Tape
                taken =
                    List.take count tape

                left : Tape
                left =
                    List.drop count tape
            in
            Ok ( taken, left )


{-| -}
parseInt : Parser Int
parseInt =
    map
        (List.foldl
            (\value acc ->
                Bitwise.shiftLeftBy 4 acc + value
            )
            0
        )
        (take intLength)


{-| -}
parseString : Parser String
parseString =
    parseInt
        |> andThen take
        |> map Elmi.Ascii.toString


{-| -}
parseList : Parser a -> Parser (List a)
parseList parser =
    parseInt |> andThen (parseListHelp parser [])


{-| -}
parseListHelp : Parser a -> List a -> Int -> Parser (List a)
parseListHelp parser acc size =
    if size == 0 then
        succeed (List.reverse acc)
    else
        parser |> andThen (\parsed -> parseListHelp parser (parsed :: acc) (size - 1))


parseUnion : List ( Int, Parser a ) -> Parser a
parseUnion choices =
    read
        |> andThen
            (\code ->
                List.foldl
                    (\( unionCode, parser ) result ->
                        if code == unionCode then
                            parser
                        else
                            result
                    )
                    (fail "Not in union.")
                    choices
            )


{-| -}
parseTuple : Parser a -> Parser b -> Parser ( a, b )
parseTuple p1 p2 =
    succeed (,)
        |= p1
        |= p2


run : Parser a -> Tape -> Result String ( a, Tape )
run (Parser parserFn) tape =
    parserFn tape


{-| -}
parseMaybe : Parser a -> Parser (Maybe a)
parseMaybe parser =
    parseUnion
        [ ( 0, succeed Nothing )
        , ( 1, map Just parser )
        ]


{-| -}
parseBool : Parser Bool
parseBool =
    parseUnion
        [ ( 0, succeed False )
        , ( 1, succeed True )
        ]


{-| -}
lazy : (() -> Parser a) -> Parser a
lazy fn =
    succeed () |> andThen fn
