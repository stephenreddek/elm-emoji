module Example exposing (suite)

import Emoji.Internal.NewParse
import Emoji.Internal.Parse exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Parsing"
        [ test "Parses 🖖" <|
            \_ ->
                let
                    result =
                        String_ [ CodeChunk [ "1f596" ] ]
                in
                Expect.equal result (Emoji.Internal.NewParse.parse "🖖")
        , test "Parses 🖖 the same" <|
            \_ ->
                Expect.equal (Emoji.Internal.NewParse.parse "🖖") (parse "🖖")
        , test "Parses 😊" <|
            \_ ->
                Expect.equal (Emoji.Internal.NewParse.parse "😊") (String_ [ CodeChunk [ "1f60a" ] ])
        , test "Parses ‼" <|
            \_ ->
                Expect.equal (Emoji.Internal.NewParse.parse "‼") (parse "‼")
        ]
