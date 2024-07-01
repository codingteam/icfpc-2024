module Lambdaman6 where

import AST
import StringBitCoding (makeRecursion)

double :: AST
double = 0 --> Concat input input
    where
    input = Var 0

times :: AST
times = makeRecursion go
    where
    self = Var 0
    action = Var 1
    input = Var 2
    iterations_left = Var 3

    go =
        0 --> -- self
        1 --> -- action
        2 --> -- input
        3 --> -- iterations_left
        (If
            (Equals iterations_left 1)
            (action $$ input)
            (action $$ (((self $$ action) $$ input) $$ (iterations_left - 1))))

lambdaman6Solution :: AST
lambdaman6Solution =
    Concat
        "solve lambdaman6 "
        (((times $$ double) $$ "R") $$ 8)
