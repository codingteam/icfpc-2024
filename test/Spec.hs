import Test.Tasty

import Test.Lib
import Test.AST
import Test.StringBitCoding
import Test.Sim3D

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [libTests, astTests, sim3DTests, stringBitCodingTests]
