module Lambdaman8 where

import qualified Data.Text as T

import AST

double :: AST
double = 0 --> Concat input input
    where
    input = Var 0

triple :: AST
triple = 0 --> Concat input (Concat input input)
    where
    input = Var 0

explode :: Char -> AST
explode c = triple $$ Str $ T.pack $ take 33 $ repeat c

goDown :: AST
goDown = explode 'D'

goLeft :: AST
goLeft = explode 'L'

goUp :: AST
goUp = explode 'U'

goRight :: AST
goRight = explode 'R'

loop :: AST
loop =
    Concat
        goDown
        (Concat
            goLeft
            (Concat
                goUp
                goRight))

spin :: AST
spin = triple $$ triple $$ triple $$ loop

lambdaman8Solution :: AST
lambdaman8Solution =
    Concat
        "solve lambdaman8 "
        spin
