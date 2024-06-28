module Test.Lib (libTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Lib

libTests :: TestTree
libTests = testGroup "Lib"
    [
        parseNumberTokenTests
    ,   parseNumberTests
    ]

parseNumberTokenTests :: TestTree
parseNumberTokenTests = testGroup "Lib.parseNumberToken"
    [
        testCase "Zero (from spec)" $ parseNumberToken "I!" @?= Just 0
    ,   testCase "One (from spec)" $ parseNumberToken "I\"" @?= Just 1
    ,   testCase "1337 (from spec)" $ parseNumberToken "I/6" @?= Just 1337
    ,   testCase "Not a number" $ parseNumberToken "!" @?= Nothing
    ,   testCase "Empty string" $ parseNumberToken "" @?= Nothing
    ,   testCase "Number with empty body" $ parseNumberToken "I" @?= Nothing
    ]

parseNumberTests :: TestTree
parseNumberTests = testGroup "Lib.parseNumberToken"
    [
        testCase "Zero (from spec)" $ parseNumber "!" @?= Just 0
    ,   testCase "One (from spec)" $ parseNumber "\"" @?= Just 1
    ,   testCase "1337 (from spec)" $ parseNumber "/6" @?= Just 1337
    ,   testCase "Empty string" $ parseNumber "" @?= Nothing
    ]
