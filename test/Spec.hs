{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit

import qualified Data.ByteString.Lazy.Char8 as BC

import PPM


tests = TestList
  [ "Line breaks in source" ~:
      TestList ["don't affect parsed value" ~: test_lineBreaksDontAffectParsedValue]
  , "Comments in source" ~:
      TestList
        [ "don't affect parsed value" ~: test_commentsDontAffectParsedValue
        , "don't affect parsed value (inline)" ~: test_commentsDontAffectParsedValueInline
        , "don't affect parsed value (semi-inline)" ~: test_commentsDontAffectParsedValueSemiInline
        ]
  ]

test_lineBreaksDontAffectParsedValue =
  let inputWithLineBreaks = BC.unlines
        [ "P3"
        , "1 1"
        , "255"
        , "0 255 0"
        ]
      inputWithoutLineBreaks = "P3 1 1 255 0 255 0"
  in parsePPM inputWithLineBreaks ~?= parsePPM inputWithoutLineBreaks

test_commentsDontAffectParsedValue =
  let inputWithComments = BC.unlines
        [ "P3 # This is a comment"
        , "1 1 ## This is another comment"
        , "255 #COMMENTTSSSS"
        , "0 255 0 #####"
        ]
      inputWithoutComments = BC.unlines
        [ "P3"
        , "1 1"
        , "255"
        , "0 255 0"
        ]
  in parsePPM inputWithComments ~?= parsePPM inputWithoutComments

test_commentsDontAffectParsedValueInline =
  let inputWithComments = "P3 1 1 255 0 255 0 # This ## is #a comment"
      inputWithoutComments = "P3 1 1 255 0 255 0"
  in parsePPM inputWithComments ~?= parsePPM inputWithoutComments

test_commentsDontAffectParsedValueSemiInline =
  let inputWithComments = BC.unlines
        [ "P3 1 1 255 # Comment"
        , "0 255 0 ## A #comment"
        ]
      inputWithoutComments = BC.unlines
        [ "P3 1 1 255"
        , "0 255 0"
        ]
  in parsePPM inputWithComments ~?= parsePPM inputWithoutComments

main :: IO ()
main = do
  runTestTT tests
  return ()
