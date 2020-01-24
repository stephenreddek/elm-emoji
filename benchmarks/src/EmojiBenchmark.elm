module Main exposing (suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Emoji
import Emoji.Internal.DictParse
import Emoji.Internal.NewParse
import Emoji.Internal.NoBacktrack
import Emoji.Internal.Parse exposing (Chunk(..), String_(..))


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    let
        longEmojiString =
            "👨\u{200D}❤️\u{200D}💋\u{200D}👨 🙇 🙇\u{1F3FE} 👨\u{200D}👩\u{200D}👧\u{200D}👦"

        simpleEmoji =
            "🖖"

        complexText =
            "Iñtërnâtiônàlizætiøn☃💩"

        complexTextWithoutEmojis =
            "Iñtërnâtiônàlizætiøn"

        longComplexText =
            String.repeat 10 (complexText ++ longEmojiString)

        longNormalText =
            String.repeat 10 complexTextWithoutEmojis
    in
    describe "Emoji"
        [ --  describe "compare original and new with backtracking"
          --     [ Benchmark.compare "text with lots of emojis" "original" (\_ -> Emoji.Internal.Parse.parse longComplexText) "new elm/parser" (\_ -> Emoji.Internal.NewParse.parse longComplexText)
          --     , Benchmark.compare "text with no emojis" "original" (\_ -> Emoji.Internal.Parse.parse longNormalText) "new elm/parser" (\_ -> Emoji.Internal.NewParse.parse longNormalText)
          --     ]
          -- ,
          describe "compare original and dict parser"
            [ Benchmark.compare "text with lots of emojis" "original" (\_ -> Emoji.Internal.Parse.parse longComplexText) "dict parser" (\_ -> Emoji.Internal.DictParse.parse longComplexText)
            , Benchmark.compare "text with no emojis" "original" (\_ -> Emoji.Internal.Parse.parse longNormalText) "dict parser" (\_ -> Emoji.Internal.DictParse.parse longNormalText)
            ]
        , describe "compare new with backtracking and dict parser"
            [ Benchmark.compare "text with lots of emojis" "new with backtracking" (\_ -> Emoji.Internal.NewParse.parse longComplexText) "dict parser" (\_ -> Emoji.Internal.DictParse.parse longComplexText)
            , Benchmark.compare "text with no emojis" "new with backtracking" (\_ -> Emoji.Internal.NewParse.parse longNormalText) "dict parser" (\_ -> Emoji.Internal.DictParse.parse longNormalText)
            ]

        -- , describe "compare original and new with NO backtracking"
        --     [ Benchmark.compare "text with lots of emojis" "original" (\_ -> Emoji.Internal.Parse.parse longComplexText) "no backtrack" (\_ -> Emoji.Internal.NoBacktrack.parse longComplexText)
        --     , Benchmark.compare "text with no emojis" "original" (\_ -> Emoji.Internal.Parse.parse longNormalText) "no backtrack" (\_ -> Emoji.Internal.NoBacktrack.parse longNormalText)
        --     ]
        -- , describe "compare new with backtracking and new with NO backtracking"
        --     [ Benchmark.compare "text with lots of emojis" "new elm/parser" (\_ -> Emoji.Internal.NewParse.parse longComplexText) "no backtrack" (\_ -> Emoji.Internal.NoBacktrack.parse longComplexText)
        --     , Benchmark.compare "text with no emojis" "new elm/parser" (\_ -> Emoji.Internal.NewParse.parse longNormalText) "no backtrack" (\_ -> Emoji.Internal.NoBacktrack.parse longNormalText)
        --     ]
        ]
