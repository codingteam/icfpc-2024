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
explode c =
    0 -->
    1 -->
    (applyThrice' $$ triple') $$ Str $ T.pack $ take 4 $ repeat c
    where
    applyThrice' = Var 0
    triple' = Var 1

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
    0 -->
    1 -->
    Concat
        ((goDown $$ applyThrice') $$ triple')
        (Concat
            ((goLeft $$ applyThrice') $$ triple')
            (Concat
                ((goUp $$ applyThrice') $$ triple')
                ((goRight $$ applyThrice') $$ triple')))
    where
    applyThrice' = Var 0
    triple' = Var 1

applyThrice :: AST
applyThrice =
    0 --> -- action
    1 --> -- input
    action $$ action $$ action $$ input
    where
    action = Var 0
    input = Var 1

spin :: AST
spin = 0 --> 1 --> (applyThrice' $$ triple') $$ ((loop $$ applyThrice') $$ triple')
    where
    applyThrice' = Var 0
    triple' = Var 1

lambdaman8Solution :: AST
lambdaman8Solution =
    Concat
        "solve lambdaman8 "
        ((spin $$ applyThrice) $$ triple)
