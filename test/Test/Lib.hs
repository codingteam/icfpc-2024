module Test.Lib (libTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Lib

libTests :: TestTree
libTests = testGroup "Lib"
    [
        testCase "Zero (from spec)" $ parseNumber "I!" @?= Just 0
    ,   testCase "One (from spec)" $ parseNumber "I\"" @?= Just 1
    ,   testCase "1337 (from spec)" $ parseNumber "I/6" @?= Just 1337
    ,   testCase "Not a number" $ parseNumber "!" @?= Nothing
    ]
