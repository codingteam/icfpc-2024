module Lambdaman6 (lambdaman6Solution) where

import AST

double :: AST
double = 0 --> Concat input input
    where
    input = Var 0

lambdaman6Solution :: AST
lambdaman6Solution =
    Concat
        "solve lambdaman6 "
        (((0 --> 1 --> (Var 0 $$ (Var 0 $$ ((Var 0) $$ ((Var 0) $$ (Var 1)))))) $$ double) $$ "RRRRRRRRRRRRR")
