module Test.Sim3D (sim3DTests) where

import Test.Tasty
import Test.Tasty.HUnit

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

doStep :: Board -> Either Sim3dError Board
doStep board = fmap s3dsCurBoard $ execSimulation simulateStep (stateFromBoard board)

basicCellPreservationTests :: TestTree
basicCellPreservationTests = testGroup "Sim3D.basicCellPreservation"
    [
        testCase "Empty cell row" $ doStep (parseBoard ". . . . .") @?= Right (parseBoard ". . . . .")
    ,   testCase "Empty cell column" $ doStep (parseBoard ".\n.\n.\n.\n.") @?= Right (parseBoard ".\n.\n.\n.\n.")
    ,   testCase "Just numbers" $ doStep (parseBoard "1 2 3 4 5") @?= Right (parseBoard "1 2 3 4 5")
    ]

autoExpandTests :: TestTree
autoExpandTests = testGroup "Sim3D.autoExpand"
    [
        testCase "Horizontal" $ doStep (parseBoard "< 1 >") @?= Right (shiftBy (-1, 0) $ parseBoard "1 < . > 1")
    ,   testCase "Vertical" $ doStep (parseBoard "^\n1\nv") @?= Right (shiftBy (0, -1) $ parseBoard "1\n^\n.\nv\n1")
    ]

shiftTests :: TestTree
shiftTests = testGroup "Sim3D.shift"
    [
        testCase "Left" $ doStep (parseBoard "1 < 2") @?= Right (parseBoard "2 < .")
    ,   testCase "Right" $ doStep (parseBoard "1 > 2") @?= Right (parseBoard ". > 1")
    ,   testCase "Up" $ doStep (parseBoard "1\n^\n2") @?= Right (parseBoard "2\n^\n.")
    ,   testCase "Down" $ doStep (parseBoard "1\nv\n2") @?= Right (parseBoard ".\nv\n1")
    ,   testCase "Operator shift" $ doStep (parseBoard "+ < -") @?= Right (parseBoard "- < .")
    ,   testCase "Simultaneous action" $ doStep (parseBoard "1 < 2 > 1") @?= Right (parseBoard "2 < . > 2")
    ]

arithmeticTests :: TestTree
arithmeticTests = testGroup "Sim3D.arithmetic"
    [
        testCase "Addition" $ doStep (parseBoard ". 1 .\n2 + .") @?= Right (parseBoard ". . .\n. + 3\n. 3 .")
    ,   testCase "Subtraction" $ doStep (parseBoard ". 2 .\n1 - .") @?= Right (parseBoard ". . .\n. - -1 .\n. -1 .")
    ,   testCase "Multiplication" $ doStep (parseBoard ". 2 .\n3 * .") @?= Right (parseBoard ". . .\n. * 6\n. 6 .")
    ,   testCase "Division" $ doStep (parseBoard ". 4 .\n6 / .") @?= Right (parseBoard ". . .\n. / 1\n. 1 .")
    ,   testCase "Modulo" $ doStep (parseBoard ". 4 .\n6 % .") @?= Right (parseBoard ". . .\n. % 2\n. 2 .")
    ,   testCase "Noop" $ doStep (parseBoard ". 4 .\n. % .") @?= Right (parseBoard ". 4 .\n. % .")
    ]

comparisonTests :: TestTree
comparisonTests = testGroup "Sim3D.comparison"
    [
        testCase "Equals (true)" $ doStep (parseBoard ". 1 .\n1 = .") @?= Right (parseBoard ". . .\n. = 1\n. 1 .")
    ,   testCase "Equals (false)" $ doStep (parseBoard ". 1 .\n2 = .") @?= Right (parseBoard ". 1 .\n2 = .")
    ,   testCase "NotEquals (true)" $ doStep (parseBoard ". 2 .\n1 # .") @?= Right (parseBoard ". . .\n. # 1\n. 2 .")
    ,   testCase "NotEquals (false)" $ doStep (parseBoard ". 2 .\n2 # .") @?= Right (parseBoard ". 2 .\n2 # .")
    ]

crashTests :: TestTree
crashTests = testGroup "Sim3D.crash"
    [
        testCase "Crash 1" $ doStep (parseBoard "1 > . < 2") @?= Right (parseBoard "1 > CRASH < 2")
    ,   testCase "Crash 2" $ doStep (parseBoard "1 > . < 1") @?= Right (parseBoard "1 > CRASH < 1")
    ]
