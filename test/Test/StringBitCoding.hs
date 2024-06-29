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
genLambdamanStep = elements (T.unpack lambdamanAlphabet)

genLambdamanSolution :: Gen T.Text
genLambdamanSolution = T.pack <$> listOf1 genLambdamanStep

newtype LambdamanSolution = LambdamanSolution T.Text
    deriving (Show)

instance Arbitrary LambdamanSolution where
    arbitrary = LambdamanSolution <$> genLambdamanSolution

genSpaceshipStep :: Gen Char
genSpaceshipStep = elements (T.unpack spaceshipAlphabet)

genSpaceshipSolution :: Gen T.Text
genSpaceshipSolution = T.pack <$> listOf1 genSpaceshipStep

newtype SpaceshipSolution = SpaceshipSolution T.Text
    deriving (Show)

instance Arbitrary SpaceshipSolution where
    arbitrary = SpaceshipSolution <$> genSpaceshipSolution

propTests :: TestTree
propTests = testGroup "Property tests"
    [
        testProperty
          "we can decode what we encoded (Lambdaman)"
          (\(LambdamanSolution s) ->
                let encoded = toSelfExtractingBitcode lambdamanAlphabet s
                    decoded = evalAst encoded
                in decoded == Right (Str s))
    ,   testProperty
          "we can decode what we encoded (Spaceship)"
          (\(SpaceshipSolution s) ->
                let encoded = toSelfExtractingBitcode spaceshipAlphabet s
                    decoded = evalAst encoded
                in decoded == Right (Str s))
    ]
