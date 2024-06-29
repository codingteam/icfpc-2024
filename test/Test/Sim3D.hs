module Test.Sim3D (sim3DTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Sim3D

sim3DTests :: TestTree
sim3DTests = testGroup "Sim3D"
    [
        basicCellPreservationTests
    ,   shiftTests
    ,   arithmeticTests
    ,   comparisonTests
    ,   crashTests
    ]

basicCellPreservationTests :: TestTree
basicCellPreservationTests = testGroup "Sim3D.basicCellPreservation"
    [
        testCase "Empty cell row" $ simulateStep (parseBoard ". . . . .") @?= parseBoard ". . . . ."
    ,   testCase "Empty cell column" $ simulateStep (parseBoard ".\n.\n.\n.\n.") @?= parseBoard ".\n.\n.\n.\n."
    ,   testCase "Just numbers" $ simulateStep (parseBoard "1 2 3 4 5") @?= parseBoard "1 2 3 4 5"
    ]

shiftTests :: TestTree
shiftTests = testGroup "Sim3D.shift"
    [
        testCase "Left" $ simulateStep (parseBoard "1 < 2") @?= parseBoard "2 < ."
    ,   testCase "Right" $ simulateStep (parseBoard "1 > 2") @?= parseBoard ". > 1"
    ,   testCase "Up" $ simulateStep (parseBoard "1\n^\n2") @?= parseBoard "2\n^\n."
    ,   testCase "Down" $ simulateStep (parseBoard "1\nv\n2") @?= parseBoard ".\nv\n1"
    ,   testCase "Operator shift" $ simulateStep (parseBoard "+ < -") @?= parseBoard "- < ."
    ,   testCase "Simultaneous action" $ simulateStep (parseBoard "1 < 2 > 1") @?= parseBoard "2 < . > 2"
    ]

arithmeticTests :: TestTree
arithmeticTests = testGroup "Sim3D.arithmetic"
    [
        testCase "Addition" $ simulateStep (parseBoard ". 1 .\n2 + .") @?= parseBoard ". . .\n. + 3\n. 3 ."
    ,   testCase "Subtraction" $ simulateStep (parseBoard ". 2 .\n1 - .") @?= parseBoard ". . .\n. - 1\n. -1 ."
    ,   testCase "Multiplication" $ simulateStep (parseBoard ". 2 .\n3 * .") @?= parseBoard ". . .\n. * 6\n. 6 ."
    ,   testCase "Division" $ simulateStep (parseBoard ". 4 .\n6 / .") @?= parseBoard ". . .\n. / 1\n. 1 ."
    ,   testCase "Modulo" $ simulateStep (parseBoard ". 4 .\n6 % .") @?= parseBoard ". . .\n. % 2\n. 2 ."
    ]

comparisonTests :: TestTree
comparisonTests = testGroup "Sim3D.comparison"
    [
        testCase "Equals (true)" $ simulateStep (parseBoard ". 1 .\n1 = .") @?= parseBoard ". . .\n. = 1\n. 1 ."
    ,   testCase "Equals (false)" $ simulateStep (parseBoard ". 1 .\n2 = .") @?= parseBoard ". 1 .\n2 = ."
    ,   testCase "NotEquals (true)" $ simulateStep (parseBoard ". 2 .\n1 # .") @?= parseBoard ". . .\n. # 1\n. 2 ."
    ,   testCase "NotEquals (false)" $ simulateStep (parseBoard ". 2 .\n2 # .") @?= parseBoard ". 2 .\n2 # ."
    ]

crashTests :: TestTree
crashTests = testGroup "Sim3D.crash"
    [
        testCase "Crash 1" $ simulateStep (parseBoard "1 > . < 2") @?= parseBoard "1 > CRASH < 2"
    ,   testCase "Crash 2" $ simulateStep (parseBoard "1 > . < 1") @?= parseBoard "1 > CRASH < 1"
    ]
