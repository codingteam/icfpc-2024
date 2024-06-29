module Test.StringBitCoding (stringBitCodingTests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.Text as T

import AST
import StringBitCoding

stringBitCodingTests :: TestTree
stringBitCodingTests = testGroup "StringBitCoding"
    [
        propTests
    ]

genLambdamanStep :: Gen Char
genLambdamanStep = elements ['L', 'R', 'U', 'D']

genLambdamanSolution :: Gen T.Text
genLambdamanSolution = T.pack <$> listOf1 genLambdamanStep

newtype LambdamanSolution = LambdamanSolution T.Text
    deriving (Show)

instance Arbitrary LambdamanSolution where
    arbitrary = LambdamanSolution <$> genLambdamanSolution

propTests :: TestTree
propTests = testGroup "Property tests"
    [
        testProperty
          "we can decode what we encoded"
          (\(LambdamanSolution s) ->
                let encoded = bitcodeString s
                    decoded = evalAst $ (Apply bitcodeDecompressor encoded)
                in decoded == Right (Str s))
    ]
