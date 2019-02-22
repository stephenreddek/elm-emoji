module Emoji.Internal.NewParse exposing (charInRange, chompCharInRange, emoji, emojiCodePoints, emojiWithText, emojiWithTextStep, exactlyChar, exactlyThenRange, fitzpatrickModifier, flagEmoji, flagPart, greatBritainEmoji, isCharInRange, keyCapEmoji, optional, parse, simpleEmoji, simpleEmojiWithOptionals, string, surrogatePair, toCodePointHex, variationSelector, variationSelectorEmoji, zeroWidthJoinerEmoji)

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
        |> Parser.run emoji
        |> Result.map List.singleton
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
            Parser.map (\chunk -> Loop (chunk :: revChunks))
    in
    Parser.oneOf
        [ addChunk (Parser.backtrackable emoji)
        , addChunk (Parser.map StringChunk (charInRange ( '\u{0000}', '힙' ))) --\u0000-\uD799
        , addChunk (Parser.map StringChunk (charInRange ( '\u{E000}', '\u{FFFF}' ))) --\uE000-\uFFFF
        , addChunk (Parser.map StringChunk surrogatePair)
        , Parser.succeed ()
            --This is where we can combine
            |> Parser.map (\_ -> Done (List.reverse revChunks))
        ]


toCodePointHex : String -> List String
toCodePointHex =
    String.foldr (\char acc -> (String.padLeft 4 '0' <| Hex.toString <| Char.toCode char) :: acc) []


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
        |= (getChompedOfLength 1 <|
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
        , Parser.backtrackable (exactlyThenRange '\u{D83C}' ( '\u{DC04}', '\u{DC04}' )) -- \ud83c[\udc04-\udc04]
        , Parser.backtrackable (exactlyThenRange '\u{D83D}' ( '\u{DC00}', '\u{DFFF}' )) -- \ud83d[\udc00-\udfff]
        , Parser.backtrackable (exactlyThenRange '\u{D83E}' ( '\u{DC00}', '\u{DFFF}' )) -- \ud83e[\udc00-\udfff]
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
    exactlyThenRange '\u{D83C}' ( '\u{DDE6}', '\u{DDFF}' )


greatBritainEmoji : Parser String
greatBritainEmoji =
    Parser.succeed (\simpleCodePoints variationPoints zeroWidthPoints -> simpleCodePoints ++ variationPoints ++ zeroWidthPoints)
        -- \ud83c\udff4\udb40\udc67\udb40\udc62
        |= string "\u{D83C}\u{DFF4}\u{DB40}\u{DC67}\u{DB40}\u{DC62}"
        |= Parser.oneOf
            [ Parser.backtrackable (string "\u{DB40}\u{DC77}\u{DB40}\u{DC6C}\u{DB40}\u{DC73}") --\udb40\udc77\udb40\udc6c\udb40\udc73
            , Parser.backtrackable (string "\u{DB40}\u{DC73}\u{DB40}\u{DC63}\u{DB40}\u{DC74}") --\udb40\udc73\udb40\udc63\udb40\udc74
            , Parser.backtrackable (string "\u{DB40}\u{DC65}\u{DB40}\u{DC6E}\u{DB40}\u{DC67}") --\udb40\udc65\udb40\udc6e\udb40\udc67
            ]
        -- \udb40\udc7f
        |= string "\u{DB40}\u{DC7F}"


string : String -> Parser String
string sequence =
    Parser.map (always sequence) (Parser.token sequence)


keyCapEmoji : Parser String
keyCapEmoji =
    let
        keyCap =
            getChompedOfLength 1 <|
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
    exactlyThenRange '\u{D83C}' ( '\u{DFFB}', '\u{DFFF}' )


variationSelector : Parser String
variationSelector =
    --\ufe0e or \ufe0f
    charInRange ( '︎', '️' )


surrogatePair : Parser String
surrogatePair =
    --TODO: Ensure this doesn't parse if its not a surrogate pair
    getChompedOfLength 2 <|
        Parser.succeed ()
            --\uD800-\uDBFF
            |. chompCharInRange ( '\u{D800}', '\u{DBFF}' )
            --\uDC00-\uDFFF
            |. chompCharInRange ( '\u{DC00}', '\u{DFFF}' )


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
    getChompedOfLength 1 (chompCharInRange range)


chompCharInRange : ( Char, Char ) -> Parser ()
chompCharInRange range =
    Parser.chompIf (isCharInRange range)


exactlyChar : Char -> Parser String
exactlyChar exact =
    getChompedOfLength 1 <|
        Parser.succeed ()
            |. Parser.chompIf ((==) exact)


exactlyThenRange : Char -> ( Char, Char ) -> Parser String
exactlyThenRange exact range =
    getChompedOfLength 2 <|
        Parser.succeed ()
            |. Parser.chompIf ((==) exact)
            |. chompCharInRange range


getChompedOfLength : Int -> Parser a -> Parser String
getChompedOfLength length parser =
    parser
        |> Parser.getChompedString
        |> Parser.andThen
            (\result ->
                if String.length result /= length then
                    Parser.problem "Not the correct length"

                else
                    Parser.succeed result
            )


combineIntoSurrogate : Char -> Char -> Char
combineIntoSurrogate first second =
    let
        left =
            Bitwise.shiftLeftBy 8 (Char.toCode first)

        right =
            Char.toCode second
    in
    Char.fromCode (left + right)
