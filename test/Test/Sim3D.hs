module Test.Sim3D (sim3DTests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T

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
    ,   timeWarpTests
    ,   outputTests
    ,   regressionTests
    ]

doStep :: Board -> Either Sim3dError Board
doStep board = fmap s3dsCurBoard $ execSimulation simulateStep (stateFromBoard board)

runStep :: Sim3dState -> Either Sim3dError Sim3dState
runStep s = fmap snd $ runSimulation simulateStep s

fromRight :: Show e => Either e a -> a
fromRight (Left err) = error $ show err
fromRight (Right value) = value

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
    ,   testCase "Empty value is not moved" $ doStep (parseBoard "1 < .") @?= Right (parseBoard "1 < .")
    ]

arithmeticTests :: TestTree
arithmeticTests = testGroup "Sim3D.arithmetic"
    [
        testCase "Addition" $ doStep (parseBoard ". 1 .\n2 + .") @?= Right (parseBoard ". . .\n. + 3\n. 3 .")
    ,   testCase "Subtraction" $ doStep (parseBoard ". 2 .\n1 - .") @?= Right (parseBoard ". . .\n. - -1 .\n. -1 .")
    ,   testCase "Multiplication" $ doStep (parseBoard ". 2 .\n3 * .") @?= Right (parseBoard ". . .\n. * 6\n. 6 .")
    ,   testCase "Division" $ doStep (parseBoard ". 4 .\n6 / .") @?= Right (parseBoard ". . .\n. / 1\n. 1 .")
    ,   testCase "Division truncates toward zero" $
            doStep (parseBoard ". 2 .\n-9 / .")
            @?= Right (parseBoard ". . .\n. / -4\n. -4 .")
    ,   testCase "Modulo" $ doStep (parseBoard ". 4 .\n6 % .") @?= Right (parseBoard ". . .\n. % 2\n. 2 .")
    ,   testCase "x%y has the same sign as x" $
            doStep (parseBoard ". 3 .\n-11 % .")
            @?= Right (parseBoard ". . .\n. % -2\n. -2 .")
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
        testCase "Crash 1" $
            doStep (parseBoard "1 > . < 2")
            @?=
            Left "[T 1] Error: trying to overwrite previously written value of \"Value 1\" with \"Value 2\" at (2,0)"
    ,   testCase "Submitting different values is not okay" $
            doStep (parseBoard "3 > S < 4")
            @?=
            Left "[T 1] Error: trying to submit \"Value 4\" when \"Value 3\" is already submitted"
    ,   testCase "Writing the same value multiple times is fine" $
            doStep (parseBoard "1 > . < 1") @?= Right (parseBoard ". > 1 < .")
    ]

timeWarpTests :: TestTree
timeWarpTests = testGroup "Sim3D.timeWarp"
    [
        testCase "Time warp (from spec)" $ do
            let initialBoard = parseBoard $ T.unlines
                    [
                        "2 > . ."
                    ,   ". 2 @ 0"
                    ,   ". . 1 ."
                    ]
            let step1 = runStep (stateFromBoard initialBoard)
            (fmap s3dsCurBoard step1) @?= Right (parseBoard $ T.unlines
                    [
                        ". > 2 ."
                    ,   ". 2 @ 0"
                    ,   ". . 1 ."
                    ])
            let step2 = runStep (fromRight step1)
            (fmap s3dsCurBoard step2) @?= Right (parseBoard $ T.unlines
                    [
                        "2 > . ."
                    ,   "2 2 @ 0"
                    ,   ". . 1 ."
                    ])
    ]

outputTests :: TestTree
outputTests = testGroup "Sim3D.output"
    [
        testCase "Submitting a value terminates the simulation" $ do
            let initialBoard = parseBoard "S < . < 42"
            let step1 = runStep (stateFromBoard initialBoard)
            (fmap s3dsCurBoard step1) @?= Right (parseBoard "S < 42 < .")
            (fmap s3dsOutput step1) @?= Right Empty
            let step2 = runStep (fromRight step1)
            (fmap s3dsOutput step2) @?= Right (Value 42)
    ,   testCase "Submitting the same value twice is okay" $ do
            let initialBoard = parseBoard "7 > S < 7"
            let step = runStep (stateFromBoard initialBoard)
            (fmap s3dsOutput step) @?= Right (Value 7)
    ]

regressionTests :: TestTree
regressionTests = testGroup "Sim3D (regression tests)"
    [
        testCase "Consuming and writing a value at the same time" $ do
            let initialBoard = parseBoard $ T.unlines
                    [
                        ". . . 3 ."
                    ,   "5 > 3 + . > S"
                    ]
            let step1 = runStep (stateFromBoard initialBoard)
            (fmap s3dsCurBoard step1) @?= (Right $ parseBoard $ T.unlines
                    [
                        "."
                    ,   ". > 5 + 6 > S"
                    ,   ". . . 6"
                    ])
    ]
