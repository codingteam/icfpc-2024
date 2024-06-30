module StringBitCoding (
    bitcodeDecompressor
,   toSelfExtractingBitcode

,   lambdamanAlphabet
,   spaceshipAlphabet
,   repeater, gRepeat
,   makeRecursion
) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import AST

lambdamanAlphabet, spaceshipAlphabet :: T.Text
lambdamanAlphabet = "UDLR"
spaceshipAlphabet = "123456789"

--- Encodes a lambdaman solution into an expression which, when evaluated,
--- produces the initial solution.
toSelfExtractingBitcode :: T.Text -> T.Text -> AST
toSelfExtractingBitcode alphabet input =
    Apply
    (Apply (bitcodeDecompressor alphabet) (bitcodeString alphabet input))
    (Number $ fromIntegral $ T.length input)

--- Encodes a lambdaman solution (a string of "UDLR" characters) as a number
--- (to be decoded on by `bitcodeDecompressor`).
bitcodeString :: T.Text -> T.Text -> AST
bitcodeString alphabet input = Number $ sum $ zipWith (\m i -> moveToDigit m * base^i) (T.unpack input) ([0..] :: [Integer])
    where
        base = fromIntegral $ T.length alphabet

        mapping = M.fromList $ zip (T.unpack alphabet) ([0..] :: [Integer])

        moveToDigit move =
            case move `M.lookup` mapping of
                Just index -> index
                Nothing -> error $ "moveToDigit: unknown move \"" ++ [move] ++ "\""

--- Galaxy program which, given a number, decompressed it into a string of
--- "RLUD" characters.
bitcodeDecompressor :: T.Text -> AST
bitcodeDecompressor alphabet = Apply yCombinator recursiveDecompressor
    where
        self = Var 0
        input_number = Var 1
        quotinent = Var 2
        decoded_char = Var 3
        base = Number $ fromIntegral $ T.length alphabet
        steps_remaining = Var 4
        recursiveDecompressor =
            Lambda 0 -- self-reference
            (Lambda 1 -- input number
                (Lambda 4 -- remaining number of steps
                    (Apply
                        (Lambda 2 -- current quotinent
                            (Apply
                                (Lambda 3 -- current decoded character
                                    (If
                                        (Equals steps_remaining (Number 1))
                                        decoded_char
                                        (Concat
                                            decoded_char
                                            (Apply
                                                (Apply self quotinent)
                                                (Sub steps_remaining (Number 1))
                                            )
                                        )
                                    )
                                )
                                (Apply decodeChar (Mod input_number base))
                            )
                        )
                        (Div input_number base)
                    )
                )
            )

        decodeChar =
          Lambda 1 -- the current remainder
          (Take (Number 1) (Drop (Var 1) (Str alphabet)))

yCombinator :: AST
yCombinator = Lambda 0 (Apply (Lambda 1 (Apply (Var 0) (Apply (Var 1) (Var 1)))) (Lambda 2 (Apply (Var 0) (Apply (Var 2) (Var 2)))))

repeater :: AST
repeater =
    Apply yCombinator (Lambda 0 (Lambda 1 (Lambda 2 (If (Equals (Var 2) (Number 1)) (Var 1) (Concat (Var 1) (Apply (Apply (Var 0) (Var 1)) (Sub (Var 2) (Number 1))))))))

gRepeat :: Integer -> AST -> AST
gRepeat n x =
    Apply (Apply (Apply yCombinator (Lambda 0 (Lambda 1 (Lambda 2 (If (Equals (Var 2) (Number 1)) (Var 1) (Concat (Var 1) (Apply (Apply (Var 0) (Var 1)) (Sub (Var 2) (Number 1))))))))) x) (Number n)

makeRecursion :: AST -> AST
makeRecursion x = Apply yCombinator x

