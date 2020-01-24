module Emoji.Internal.DictParse exposing (parse)

import Bitwise
import Dict
import Emoji.Internal.Parse exposing (Chunk(..), String_(..))
import Emoji.Internal.Valid
import Hex
import List
import Parser exposing ((|.), (|=), Parser, Step(..))
import Parser.Dict
import String


parse : String -> String_
parse text =
    text
        |> Parser.run emojiWithText
        |> Result.withDefault [ StringChunk text ]
        |> String_


emojiWithText : Parser (List Chunk)
emojiWithText =
    Parser.loop [] emojiWithTextStep


emojiParser : Parser (List String)
emojiParser =
    Emoji.Internal.Valid.pairs
        |> Dict.fromList
        |> Parser.Dict.fromDict


emojiWithTextStep : List Chunk -> Parser (Step (List Chunk) (List Chunk))
emojiWithTextStep revChunks =
    let
        addChunk =
            -- Can maybe combine string chunks here too
            Parser.backtrackable >> Parser.map (\chunk -> Loop (chunk :: revChunks))

        combineStrings : Chunk -> ( Maybe Chunk, List Chunk ) -> ( Maybe Chunk, List Chunk )
        combineStrings cur ( last, acc ) =
            case ( cur, last ) of
                ( CodeChunk _, Just chunk ) ->
                    ( Just cur, chunk :: acc )

                ( CodeChunk _, Nothing ) ->
                    ( Just cur, acc )

                ( StringChunk currString, Just (StringChunk accStrings) ) ->
                    ( Just (StringChunk (currString ++ accStrings)), acc )

                ( StringChunk _, Just chunk ) ->
                    ( Just cur, chunk :: acc )

                ( StringChunk _, Nothing ) ->
                    ( Just cur, acc )

        addLast ( last, acc ) =
            case last of
                Just chunk ->
                    chunk :: acc

                Nothing ->
                    acc
    in
    Parser.oneOf
        [ addChunk (Parser.map CodeChunk emojiParser)
        , addChunk (Parser.map StringChunk (charInRange ( '\u{0000}', '힙' ))) --\u0000-\uD799
        , addChunk (Parser.map StringChunk (charInRange ( '\u{E000}', '\u{FFFF}' ))) --\uE000-\uFFFF
        , addChunk (Parser.map StringChunk surrogatePair)
        , Parser.succeed ()
            |> Parser.map (\_ -> Done (List.foldl combineStrings ( Nothing, [] ) revChunks |> addLast))
        ]


isCharInRange : ( Char, Char ) -> Char -> Bool
isCharInRange ( low, high ) toTest =
    low <= toTest && high >= toTest


charInRange : ( Char, Char ) -> Parser String
charInRange range =
    Parser.getChompedString (chompCharInRange range)


surrogatePair : Parser String
surrogatePair =
    --TODO: Ensure this doesn't parse if its not a surrogate pair
    Parser.getChompedString <|
        --\uD800-\uDBFF (first half)
        --\uDC00-\uDFFF (second half)
        chompCharInRange
            ( combineIntoSurrogate 0xD800 0xDC00
            , combineIntoSurrogate 0xDBFF 0xDFFF
            )


chompCharInRange : ( Char, Char ) -> Parser ()
chompCharInRange range =
    Parser.chompIf (isCharInRange range)


combineIntoSurrogate : Int -> Int -> Char
combineIntoSurrogate high low =
    -- https://mathiasbynens.be/notes/javascript-encoding#surrogate-formulae
    Char.fromCode ((high - 0xD800) * 0x0400 + low - 0xDC00 + 0x00010000)
