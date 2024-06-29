module Test.Sim3D (sim3DTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Control.Monad.State

import Sim3D

sim3DTests :: TestTree
sim3DTests = testGroup "Sim3D"
    [
        basicCellPreservationTests
    ,   shiftTests
    ,   autoExpandTests
    ,   arithmeticTests
    ,   comparisonTests
    ,   crashTests
    ]

doStep :: Board -> Board
doStep board = s3dsCurBoard $ execState simulateStep (stateFromBoard board)

basicCellPreservationTests :: TestTree
basicCellPreservationTests = testGroup "Sim3D.basicCellPreservation"
    [
        testCase "Empty cell row" $ doStep (parseBoard ". . . . .") @?= parseBoard ". . . . ."
    ,   testCase "Empty cell column" $ doStep (parseBoard ".\n.\n.\n.\n.") @?= parseBoard ".\n.\n.\n.\n."
    ,   testCase "Just numbers" $ doStep (parseBoard "1 2 3 4 5") @?= parseBoard "1 2 3 4 5"
    ]

autoExpandTests :: TestTree
autoExpandTests = testGroup "Sim3D.autoExpand"
    [
        testCase "Horizontal" $ doStep (parseBoard "< 1 >") @?= (shiftBy (-1, 0) $ parseBoard "1 < . > 1")
    ,   testCase "Vertical" $ doStep (parseBoard "^\n1\nv") @?= (shiftBy (0, -1) $ parseBoard "1\n^\n.\nv\n1")
    ]

shiftTests :: TestTree
shiftTests = testGroup "Sim3D.shift"
    [
        testCase "Left" $ doStep (parseBoard "1 < 2") @?= parseBoard "2 < ."
    ,   testCase "Right" $ doStep (parseBoard "1 > 2") @?= parseBoard ". > 1"
    ,   testCase "Up" $ doStep (parseBoard "1\n^\n2") @?= parseBoard "2\n^\n."
    ,   testCase "Down" $ doStep (parseBoard "1\nv\n2") @?= parseBoard ".\nv\n1"
    ,   testCase "Operator shift" $ doStep (parseBoard "+ < -") @?= parseBoard "- < ."
    ,   testCase "Simultaneous action" $ doStep (parseBoard "1 < 2 > 1") @?= parseBoard "2 < . > 2"
    ]

arithmeticTests :: TestTree
arithmeticTests = testGroup "Sim3D.arithmetic"
    [
        testCase "Addition" $ doStep (parseBoard ". 1 .\n2 + .") @?= parseBoard ". . .\n. + 3\n. 3 ."
    ,   testCase "Subtraction" $ doStep (parseBoard ". 2 .\n1 - .") @?= parseBoard ". . .\n. - -1 .\n. -1 ."
    ,   testCase "Multiplication" $ doStep (parseBoard ". 2 .\n3 * .") @?= parseBoard ". . .\n. * 6\n. 6 ."
    ,   testCase "Division" $ doStep (parseBoard ". 4 .\n6 / .") @?= parseBoard ". . .\n. / 1\n. 1 ."
    ,   testCase "Modulo" $ doStep (parseBoard ". 4 .\n6 % .") @?= parseBoard ". . .\n. % 2\n. 2 ."
    ,   testCase "Noop" $ doStep (parseBoard ". 4 .\n. % .") @?= parseBoard ". 4 .\n. % ."
    ]

comparisonTests :: TestTree
comparisonTests = testGroup "Sim3D.comparison"
    [
        testCase "Equals (true)" $ doStep (parseBoard ". 1 .\n1 = .") @?= parseBoard ". . .\n. = 1\n. 1 ."
    ,   testCase "Equals (false)" $ doStep (parseBoard ". 1 .\n2 = .") @?= parseBoard ". 1 .\n2 = ."
    ,   testCase "NotEquals (true)" $ doStep (parseBoard ". 2 .\n1 # .") @?= parseBoard ". . .\n. # 1\n. 2 ."
    ,   testCase "NotEquals (false)" $ doStep (parseBoard ". 2 .\n2 # .") @?= parseBoard ". 2 .\n2 # ."
    ]

crashTests :: TestTree
crashTests = testGroup "Sim3D.crash"
    [
        testCase "Crash 1" $ doStep (parseBoard "1 > . < 2") @?= parseBoard "1 > CRASH < 2"
    ,   testCase "Crash 2" $ doStep (parseBoard "1 > . < 1") @?= parseBoard "1 > CRASH < 1"
    ]
