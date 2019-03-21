module Example exposing (suite)

import Emoji.Internal.NewParse
import Emoji.Internal.Parse exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser
import Test exposing (..)


testNewAgainstOld : String -> Test
testNewAgainstOld testText =
    test ("New Parser parses \"" ++ testText ++ "\" the same as the old parser") <|
        \_ ->
            Expect.equal (Emoji.Internal.NewParse.parse testText) (Emoji.Internal.Parse.parse testText)


testNewAgainstExpected : String -> String_ -> Test
testNewAgainstExpected testText expected =
    test ("New Parser correctly parses \"" ++ testText ++ "\"") <|
        \_ ->
            Expect.equal expected (Emoji.Internal.NewParse.parse testText)


testOldAgainstExpected : String -> String_ -> Test
testOldAgainstExpected testText expected =
    test ("Old Parser correctly parses \"" ++ testText ++ "\"") <|
        \_ ->
            Expect.equal expected (Emoji.Internal.Parse.parse testText)


testBothAgainstExpected : String -> String_ -> List Test
testBothAgainstExpected testText expected =
    [ testNewAgainstExpected testText expected
    , testOldAgainstExpected testText expected
    ]


suite : Test
suite =
    describe "Parsing" <|
        [ testNewAgainstOld "🖖"
        , testNewAgainstOld "‼"
        , testNewAgainstOld "7️⃣"
        , testNewAgainstOld "🔟"
        , testNewAgainstOld "👨\u{200D}❤️\u{200D}💋\u{200D}👨"
        , testNewAgainstOld "beginning 🖖 ending"
        , testNewAgainstOld "👨\u{200D}❤️\u{200D}💋\u{200D}👨 🙇 🙇\u{1F3FE} 👨\u{200D}👩\u{200D}👧\u{200D}👦"
        , testNewAgainstExpected "\u{1F939}\u{1F3FD}\u{200D}♀" (String_ [ CodeChunk [ "1f939", "1f3fd", "200d", "2640" ] ])
        , testNewAgainstExpected "🏊\u{1F3FF}\u{200D}♀" (String_ [ CodeChunk [ "1f3ca", "1f3ff", "200d", "2640" ] ])
        , testNewAgainstExpected "🏊\u{200D}♂️" (String_ [ CodeChunk [ "1f3ca", "200d", "2642", "fe0f" ] ])
        ]
            ++ testBothAgainstExpected "😊" (String_ [ CodeChunk [ "1f60a" ] ])
            ++ testBothAgainstExpected "🖖" (String_ [ CodeChunk [ "1f596" ] ])
            ++ testBothAgainstExpected "Iñtërnâtiônàlizætiøn☃💩" (String_ [ StringChunk "Iñtërnâtiônàlizætiøn", CodeChunk [ "2603" ], CodeChunk [ "1f4a9" ] ])
            ++ testBothAgainstExpected "🏊" (String_ [ CodeChunk [ "1f3ca" ] ])
