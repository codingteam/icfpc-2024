module StringBitCoding (
    bitcodeDecompressor
,   bitcodeString
,   toSelfExtractingBitcode
) where

import qualified Data.Text as T

import AST

--- Encodes a lambdaman solution into an expression which, when evaluated,
--- produces the initial solution.
toSelfExtractingBitcode :: T.Text -> AST
toSelfExtractingBitcode input =
    Apply
    (Apply bitcodeDecompressor (bitcodeString input))
    (Number $ fromIntegral $ T.length input)

--- Encodes a lambdaman solution (a string of "UDLR" characters) as a number
--- (to be decoded on by `bitcodeDecompressor`).
bitcodeString :: T.Text -> AST
bitcodeString input = Number $ sum $ zipWith (\m i -> moveToDigit m * base^i) (T.unpack input) ([0..] :: [Integer])
    where
        base = 4

        moveToDigit 'U' = 0
        moveToDigit 'D' = 1
        moveToDigit 'L' = 2
        moveToDigit 'R' = 3
        moveToDigit c = error $ "moveToDigit: unknown move \"" ++ [c] ++ "\""

--- Galaxy program which, given a number, decompressed it into a string of
--- "RLUD" characters.
bitcodeDecompressor :: AST
bitcodeDecompressor = Apply yCombinator recursiveDecompressor
    where
        self = Var 0
        input_number = Var 1
        quotinent = Var 2
        decoded_char = Var 3
        base = Number 4
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
          (Take (Number 1) (Drop (Var 1) (Str "UDLR")))

yCombinator :: AST
yCombinator = Lambda 0 (Apply (Lambda 1 (Apply (Var 0) (Apply (Var 1) (Var 1)))) (Lambda 2 (Apply (Var 0) (Apply (Var 2) (Var 2)))))
