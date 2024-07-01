module Lambdaman9 (lambdaman9Solution) where

import AST

double :: AST
double = 0 --> Concat input input
    where
    input = Var 0

goRight :: AST
goRight = double $$ "RRRRRRRRRRRRRRRRRRRRRRRRR"

goLeft :: AST
goLeft = double $$ "LLLLLLLLLLLLLLLLLLLLLLLLL"

goDown :: AST
goDown = Str "D"

step :: AST
step =
    Concat
        goRight
        (Concat
            goDown
            (Concat
                goLeft
                goDown))

lambdaman9Solution :: AST
lambdaman9Solution =
    Concat
        "solve lambdaman9 "
        (((0 --> 1 --> (Var 0 $$ (Var 0 $$ (Var 0 $$ (Var 0 $$ (Var 0 $$ Var 1)))))) $$ double) $$ step)
