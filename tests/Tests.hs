{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec
import qualified Data.ByteString    as B
import           Data.Monoid
import           Test.Tasty
import           Test.Tasty.HUnit

import           Graphics.PDF.Parse
import           Graphics.PDF.Types

assertParse :: (Eq a, Show a) => Parser a -> B.ByteString -> a -> Assertion
assertParse p b a = case parseOnly p b of
    Left  e -> assertFailure $ "parse of " ++ show b ++ " failed: " ++ e
    Right r -> r @?= a

assertObjectParse :: (a -> Object) -> B.ByteString -> a -> Assertion
assertObjectParse f b = assertParse object b . f

(@?~) :: B.ByteString -> Object -> Assertion
(@?~) = assertParse object

infixr 0 @?~

tests :: TestTree
tests = testGroup "parsing"
    [ objectTests
    , fileTests
    ]

objectTests :: TestTree
objectTests = testGroup "objects"
    [ testCase "comments" $
        "[123% comment {/%} blah blah blah\n456]" @?~
            array [Number (I 123), Number (I 456)]
    , testCase "null object" $
        "null" @?~ Null
    , testCase "boolean objects" $ do
        "true"  @?~ Bool True
        "false" @?~ Bool False
    , testCase "numeric objects" $ do
        let (@?~>) = assertObjectParse Number
        "123"    @?~> I 123
        "43445"  @?~> I 43445
        "+17"    @?~> I 17
        "-98"    @?~> I (-98)
        "0"      @?~> I 0
        "34.5"   @?~> F 34.5
        "-3.62"  @?~> F (-3.62)
        "+123.6" @?~> F 123.6
        "4."     @?~> F 4
        "-.002"  @?~> F (-0.002)
        "0.0"    @?~> F 0
    , testCase "literal strings" $ do
        let (@?~>) = assertObjectParse String
        "(a string)"                  @?~> "a string"
        "()"                          @?~> ""
        "(\n)"                        @?~> "\n"
        "((())())"                    @?~> "(())()"
        "(%*!&})"                     @?~> "%*!&}"
        "(\\))"                       @?~> ")"
        "(\\n\\r\\t\\b\\f\\(\\)\\\\)" @?~> "\n\r\t\b\f()\\"
        "(\\\n)"                      @?~> ""
        "(\\a)"                       @?~> "a"
        "(\\245\\307)"                @?~> "\o245\o307"
        "(\\0053)"                    @?~> "\o005\&3"
        "(\\53)"                      @?~> "\o53"
        "(\xbe)"                      @?~> "\xbe"
    , testCase "hexadecimal strings" $ do
        let (@?~>) = assertObjectParse String
        "<901FA3>" @?~> "\x90\x1f\xa3"
        "<901fa3>" @?~> "\x90\x1f\xa3"
        "<90\ta3>" @?~> "\x90\xa3"
        "<901FA>"  @?~> "\x90\x1f\xa0"
    , testCase "name objects" $ do
        let (@?~>) = assertObjectParse Name
        "/Name"      @?~> "Name"
        "/ Name"     @?~> ""
        "/.@$;_*-"   @?~> ".@$;_*-"
        "/A#20B"     @?~> "A B"
        "/F#23"      @?~> "F#"
        "/Name/Name" @?~> "Name"
        "/Na%me"     @?~> "Na"
    , testCase "array objects" $ do
        "[549 3.14 false (Ralph) /SomeName]" @?~
            array [ Number (I 549), Number (F 3.14), Bool False
                  , String "Ralph", Name "SomeName"] 
        "[ [ true false ] null ]" @?~
            array [array [Bool True, Bool False], Null]
    , testCase "dictionary objects" $
        let s = "<< /Type /Example          " <>
                "   /Version 0.01           " <>
                "   /String (a string)      " <>
                "   /SubDict << /Item1 4    " <>
                "               /Item2 true " <>
                "            >>             " <>
                ">>                         "
            r = dict [ ("Type"   , Name "Example")
                     , ("Version", Number (F 0.01))
                     , ("String" , String "a string")
                     , ("SubDict", dict [ ("Item1", Number (I 4))
                                        , ("Item2", Bool True)
                                        ])
                     ]
        in s @?~ r
    , testCase "indirect objects" $
        "12 0 obj (Brillig) endobj" @?~
            Indirect (ObjectID 12 0) (String "Brillig")
    , testCase "references" $ do
        "12 0 R" @?~ Reference (ObjectID 12 0)
        "<< /Length 8 0 R /Filter /LZWDecode >>" @?~
            dict [ ("Length", Reference (ObjectID 8 0))
                 , ("Filter", Name "LZWDecode")
                 ]
    ]

fileTests :: TestTree
fileTests = testGroup "file structure"
    [ testCase "version" $
        assertParse version "%PDF-1.7" (1, 7)
    , testCase "cross-reference offset" $ do
        assertParse xrefOffset
            " >>\nstartxref\n18799\n%%EOF"
            18799
        assertParse xrefOffset
            "16 /Name % startxref 142 %%EOF\nstartxref\n18799\n%%EOF"
            18799
    ]

main :: IO ()
main = defaultMain tests
