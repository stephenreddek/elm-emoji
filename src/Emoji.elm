module Emoji exposing
    ( text_, fromEmoji
    , textWith, fromEmojiWith, replaceWithEmojiOne, replaceWithTwemoji, removeJoiners
    )

{-| This library is for conveniently supporting
[emoji](http://unicode.org/emoji/charts/full-emoji-list.html) in Elm
applications.

There is a high-level drop-in replacement for `Html.text` which has to make
some extra assumptions about the app, and customizable mapping over emojis.


# The high level

@docs text_, fromEmoji


# Customizable

@docs textWith, fromEmojiWith, replaceWithEmojiOne, replaceWithTwemoji, removeJoiners, removeVariationSelectors

-}

import Emoji.Internal.Parse exposing (..)
import Emoji.Internal.Valid
import Html exposing (..)
import Html.Attributes exposing (..)
import List
import String


{-| Convert a String with unicode emoji characters into an Html element
containing the text with `<img>` tags replacing the emojis.

This function produces a `<span class='elm-emoji'>` containing the text, replacing
emojis with `<img class='elm-emoji-img elm-emoji-one'>` tags pointing to CDN-hosted
[EmojiOne](http://emojione.com/).

    div [] [ text_ "Live long and prosper 🖖" ]

-}
text_ : String -> Html a
text_ =
    textWith replaceWithEmojiOne >> span [ class "elm-emoji" ]


{-| Create a customized emoji converter. The function argument maps emoji
(identified by the lowercase hex-encoded unicode code point sequence) to
Html nodes.

    mapEmoji : List String -> Html a
    mapEmoji codePoints =
        text ("(I'm code " ++ (String.join "-" codePoints) ++ ")")

    div []
        ( textWith mapEmoji "here's a penguin:🐧" )

-}
textWith : ({ codepts : List String, shortname : String } -> Html a) -> String -> List (Html a)
textWith replacer body =
    let
        (String_ chunks) =
            parse body
    in
    List.map
        (\chunk ->
            case chunk of
                StringChunk s ->
                    text s

                CodeChunk emoji ->
                    replacer emoji
        )
        chunks


fromEmoji : String -> Maybe (Html a)
fromEmoji =
    fromEmojiWith replaceWithEmojiOne


fromEmojiWith : ({ codepts : List String, shortname : String } -> Html a) -> String -> Maybe (Html a)
fromEmojiWith replacer emoji =
    case Emoji.Internal.Parse.splitPrefix emoji of
        ( ( 0, _, _ ), _ ) ->
            Nothing

        ( ( matchLen, matchCodes, matchShortName ), remaining ) ->
            if String.length remaining > 0 then
                --What to do with remaining?
                --We could say that it doesn't match at all if it isn't a complete match and return Nothing
                --That strategy doesn't work, however, with this at the end "♂️" but only with "♂" I believe the difference
                -- is the emoji selector and I don't want that to be the issue... perhaps leave it for now.
                Nothing

            else
                Just (replacer { codepts = matchCodes, shortname = matchShortName })


{-| Turn an emoji unicode sequence into an `<img>` pointing at
[EmojiOne](http://emojione.com/), with classes `elm-emoji-img` and `elm-emoji-one`.

    text_ : String -> Html a
    text_ =
        textWith replaceWithEmojiOne >> span [ class "elm-emoji" ]

-}
replaceWithEmojiOne : { codepts : List String, shortname : String } -> Html a
replaceWithEmojiOne emoji =
    img
        [ src <| urlWithBase emojiOneV4BaseUrl <| removeVariationSelectors <| removeJoiners emoji.codepts
        , class "elm-emoji-img elm-emoji-one"
        , title emoji.shortname
        ]
        []


{-| Convert an emoji unicode sequence into a
[Twemoji](http://twitter.github.io/twemoji/) `<img>` tag. It will have CSS
classes `elm-emoji-img` and `elm-emoji-twem`.

    text_ : String -> Html a
    text_ body =
        span [] (textWith replaceWithTwemoji body)

-}
replaceWithTwemoji : { codepts : List String, shortname : String } -> Html a
replaceWithTwemoji codepts =
    img
        [ src <| urlWithBase twemojiBaseUrl codepts
        , class "elm-emoji-img elm-emoji-twem"
        , title emoji.shortname
        ]
        []


{-| EmojiOne file names require the zero-width-joiners and variation selectors to be removed
-}
removeJoiners : { codepts : List String, shortname : String } -> List String
removeJoiners =
    let
        isJoiner c =
            c == "200D"
    in
    List.filter (String.toUpper >> isJoiner >> not)


{-| EmojioOe file names require the variation selectors to be removed
-}
removeVariationSelectors : { codepts : List String, shortname : String } -> List String
removeVariationSelectors =
    let
        isSelector c =
            c == "FE0F" || c == "FE0E"
    in
    List.filter (String.toUpper >> isSelector >> not)


urlWithBase : String -> List String -> String
urlWithBase base codepts =
    base ++ String.join "-" codepts ++ ".png"


emojiOneV2BaseUrl : String
emojiOneV2BaseUrl =
    "https://cdnjs.cloudflare.com/ajax/libs/emojione/2.2.6/assets/png/"


emojiOneV3BaseUrl : String
emojiOneV3BaseUrl =
    "https://cdn.jsdelivr.net/emojione/assets/3.1/png/64/"


emojiOneV4BaseUrl : String
emojiOneV4BaseUrl =
    "https://cdn.jsdelivr.net/emojione/assets/4.5/png/64/"


twemojiBaseUrl : String
twemojiBaseUrl =
    "https://twemoji.maxcdn.com/2/72x72/"
