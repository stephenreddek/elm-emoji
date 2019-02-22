module Emoji.Internal.NewParse exposing (charInRange, chompCharInRange, emoji, emojiCodePoints, emojiWithText, emojiWithTextStep, exactlyChar, fitzpatrickModifier, flagEmoji, flagPart, greatBritainEmoji, isCharInRange, keyCapEmoji, optional, parse, simpleEmoji, simpleEmojiWithOptionals, surrogatePair, toCodePointHex, variationSelector, variationSelectorEmoji, zeroWidthJoinerEmoji)

import Bitwise
import Dict
import Emoji.Internal.Parse exposing (Chunk(..), String_(..))
import Emoji.Internal.Valid exposing (Store(..), longest, store)
import Hex
import List
import Parser exposing ((|.), (|=), Parser, Step(..))
import String



--TODO: only expose parse later


parse : String -> String_
parse text =
    text
        |> Parser.run emojiWithText
        |> Result.mapError (Debug.log "err")
        |> Result.withDefault [ StringChunk text ]
        |> String_


emojiWithText : Parser (List Chunk)
emojiWithText =
    Parser.loop [] emojiWithTextStep


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
        [ addChunk emoji
        , addChunk (Parser.map StringChunk (charInRange ( '\u{0000}', '힙' ))) --\u0000-\uD799
        , addChunk (Parser.map StringChunk (charInRange ( '\u{E000}', '\u{FFFF}' ))) --\uE000-\uFFFF
        , addChunk (Parser.map StringChunk surrogatePair)
        , Parser.succeed ()
            |> Parser.map (\_ -> Done (List.foldl combineStrings ( Nothing, [] ) revChunks |> addLast))
        ]


toCodePointHex : String -> List String
toCodePointHex =
    String.foldr (\char acc -> (Hex.toString <| Char.toCode char) :: acc) []


emoji : Parser Chunk
emoji =
    Parser.succeed (\codePoints modifier -> CodeChunk (toCodePointHex (codePoints ++ modifier)))
        |= emojiCodePoints
        |= optional fitzpatrickModifier ""


emojiCodePoints : Parser String
emojiCodePoints =
    Parser.oneOf
        [ Parser.backtrackable variationSelectorEmoji
        , Parser.backtrackable zeroWidthJoinerEmoji
        , Parser.backtrackable simpleEmojiWithOptionals
        , Parser.backtrackable keyCapEmoji
        ]


variationSelectorEmoji : Parser String
variationSelectorEmoji =
    Parser.succeed (\codePoints variationPoints -> codePoints ++ variationPoints)
        |= (Parser.getChompedString <|
                -- '\u00a9' // trademark
                -- '\u00ae' // copyright
                -- '\u3030' // 〰
                -- '\u303d' // 〽
                Parser.chompIf (\c -> c == '©' || c == '®' || c == '〰' || c == '〽')
           )
        |= variationSelector


zeroWidthJoinerEmoji : Parser String
zeroWidthJoinerEmoji =
    let
        joiner =
            Parser.succeed (\joinerPoints emojiPoints selectorPoints -> joinerPoints ++ emojiPoints ++ selectorPoints)
                -- '\u200d'
                |= exactlyChar '\u{200D}'
                |= simpleEmoji
                |= optional variationSelector ""

        --TODO: implement this
        upToTwoMoreJoiners =
            Parser.succeed (\first second -> first ++ second)
                |= optional joiner ""
                |= optional joiner ""
    in
    Parser.succeed (\simpleCodePoints variationPoints zeroWidthPoints moreJoiners -> simpleCodePoints ++ variationPoints ++ zeroWidthPoints ++ moreJoiners)
        |= simpleEmoji
        |= optional variationSelector ""
        |= joiner
        |= upToTwoMoreJoiners


simpleEmojiWithOptionals : Parser String
simpleEmojiWithOptionals =
    Parser.succeed (\codePoints modifier selector -> codePoints ++ modifier ++ selector)
        |= simpleEmoji
        |= optional fitzpatrickModifier ""
        |= optional variationSelector ""


simpleEmoji : Parser String
simpleEmoji =
    Parser.oneOf
        [ Parser.backtrackable (charInRange ( '‼', '\u{2BFF}' )) -- \u203c-\u2bff
        , Parser.backtrackable (charInRange ( '✂', '➰' )) -- \u2702-\u27b0
        , Parser.backtrackable (charInRange ( '㈀', '\u{32FF}' )) -- \u3200-\u32ff
        , Parser.backtrackable flagEmoji
        , Parser.backtrackable greatBritainEmoji
        , Parser.backtrackable (charInRange ( combineIntoSurrogate 0xD83C 0xDC04, combineIntoSurrogate 0xD83C 0xDC04 )) -- \ud83c[\udc04-\udc04]
        , Parser.backtrackable (charInRange ( combineIntoSurrogate 0xD83D 0xDC00, combineIntoSurrogate 0xD83D 0xDFFF )) -- \ud83d[\udc00-\udfff]
        , Parser.backtrackable (charInRange ( combineIntoSurrogate 0xD83E 0xDC00, combineIntoSurrogate 0xD83E 0xDFFF )) -- \ud83e[\udc00-\udfff]
        ]


flagEmoji : Parser String
flagEmoji =
    Parser.succeed (\part1 part2 -> part1 ++ part2)
        |= flagPart
        |= flagPart


flagPart : Parser String
flagPart =
    -- \ud83c
    -- \udde6-\uddff
    charInRange ( combineIntoSurrogate 0xD83C 0xDDE6, combineIntoSurrogate 0xD83C 0xDDFF )


greatBritainEmoji : Parser String
greatBritainEmoji =
    Parser.succeed (\simpleCodePoints variationPoints zeroWidthPoints -> simpleCodePoints ++ variationPoints ++ zeroWidthPoints)
        -- \ud83c\udff4\udb40\udc67\udb40\udc62
        |= surrogateString [ combineIntoSurrogate 0xD83C 0xDFF4, combineIntoSurrogate 0xDB40 0xDC67, combineIntoSurrogate 0xDB40 0xDC62 ]
        |= Parser.oneOf
            [ Parser.backtrackable (surrogateString [ combineIntoSurrogate 0xDB40 0xDC77, combineIntoSurrogate 0xDB40 0xDC6C, combineIntoSurrogate 0xDB40 0xDC73 ]) --\udb40\udc77\udb40\udc6c\udb40\udc73]
            , Parser.backtrackable (surrogateString [ combineIntoSurrogate 0xDB40 0xDC73, combineIntoSurrogate 0xDB40 0xDC63, combineIntoSurrogate 0xDB40 0xDC74 ]) --\udb40\udc73\udb40\udc63\udb40\udc74]
            , Parser.backtrackable (surrogateString [ combineIntoSurrogate 0xDB40 0xDC65, combineIntoSurrogate 0xDB40 0xDC6E, combineIntoSurrogate 0xDB40 0xDC67 ]) --\udb40\udc65\udb40\udc6e\udb40\udc67]
            ]
        -- \udb40\udc7f
        |= surrogateString [ combineIntoSurrogate 0xDB40 0xDC7F ]


surrogateString : List Char -> Parser String
surrogateString sequenceParts =
    let
        sequence =
            String.fromList sequenceParts
    in
    Parser.map (always sequence) (Parser.token sequence)


keyCapEmoji : Parser String
keyCapEmoji =
    let
        keyCap =
            Parser.getChompedString <|
                -- 0-9, #, *
                Parser.chompIf (\c -> (c >= '0' && c <= '9') || c == '#' || c == '*')
    in
    Parser.succeed (\keyCapString modifier selector -> keyCapString ++ modifier ++ selector)
        |= keyCap
        |= optional variationSelector ""
        -- \u20e3
        |= exactlyChar '⃣'


fitzpatrickModifier : Parser String
fitzpatrickModifier =
    --\ud83c
    --\udffb-\udfff
    charInRange ( combineIntoSurrogate 0xD83C 0xDFFB, combineIntoSurrogate 0xD83C 0xDFFF )


variationSelector : Parser String
variationSelector =
    --\ufe0e or \ufe0f
    charInRange ( '︎', '️' )


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


optional : Parser a -> a -> Parser a
optional toAttempt default =
    Parser.oneOf
        [ Parser.backtrackable toAttempt
        , Parser.succeed default
        ]


isCharInRange : ( Char, Char ) -> Char -> Bool
isCharInRange ( low, high ) toTest =
    low <= toTest && high >= toTest


charInRange : ( Char, Char ) -> Parser String
charInRange range =
    Parser.getChompedString (chompCharInRange range)


chompCharInRange : ( Char, Char ) -> Parser ()
chompCharInRange range =
    Parser.chompIf (isCharInRange range)


exactlyChar : Char -> Parser String
exactlyChar exact =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf ((==) exact)


combineIntoSurrogate : Int -> Int -> Char
combineIntoSurrogate high low =
    -- https://mathiasbynens.be/notes/javascript-encoding#surrogate-formulae
    Char.fromCode ((high - 0xD800) * 0x0400 + low - 0xDC00 + 0x00010000)
