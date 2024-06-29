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
toSelfExtractingBitcode input = Apply bitcodeDecompressor (bitcodeString input)

--- Encodes a lambdaman solution (a string of "UDLR" characters) as a number
--- (to be decoded on by `bitcodeDecompressor`).
bitcodeString :: T.Text -> AST
bitcodeString input = Number $ sum $ zipWith (\m i -> moveToDigit m * base^i) (T.unpack input) ([0..] :: [Integer])
    where
        base = 5

        moveToDigit 'U' = 1
        moveToDigit 'D' = 2
        moveToDigit 'L' = 3
        moveToDigit 'R' = 4
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
        base = Number 5
        recursiveDecompressor =
            Lambda 0 -- self-reference
            (Lambda 1 -- input number
                (Apply
                    (Lambda 2 -- current quotinent
                        (Apply
                            (Lambda 3 -- current decoded character
                                (If
                                    (Equals quotinent (Number 0))
                                    decoded_char
                                    (Concat
                                        decoded_char
                                        (Apply self quotinent))
                                )
                            )
                            (Apply decodeChar (Mod input_number base))
                        )
                    )
                    (Div input_number base)
                )
            )

        decodeChar =
          Lambda 1 -- the current remainder
          (Drop (Sub (Var 1) (Number 1))
            (Take (Var 1) (Str "UDLR")))

yCombinator :: AST
yCombinator = Lambda 0 (Apply (Lambda 1 (Apply (Var 0) (Apply (Var 1) (Var 1)))) (Lambda 2 (Apply (Var 0) (Apply (Var 2) (Var 2)))))
