import Test.Tasty

import Test.Lib
import Test.AST
import Test.StringBitCoding

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [libTests, astTests, stringBitCodingTests]
