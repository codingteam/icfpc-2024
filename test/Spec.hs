import Test.Tasty

import Test.Lib

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [libTests]
