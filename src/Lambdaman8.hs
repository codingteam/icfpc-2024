module Lambdaman8 where

import qualified Data.Text as T

import AST
import StringBitCoding (makeRecursion)

double :: AST
double = 0 --> Concat input input
    where
    input = Var 0

times :: AST
times = 10 --> makeRecursion go
    where
    self = Var 0
    action = Var 10
    iterations_left = Var 2

    go =
        0 --> -- self
        2 --> -- iterations_left
        (If
            (Equals iterations_left 1)
            (action)
            (Concat
              (self $$ (iterations_left-1))
              (action)))

explode :: Char -> AST
explode c = double $$ double $$ Str $ T.pack $ take 25 $ repeat c

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
spin = double $$ double $$ double $$ double $$ double $$ loop

lambdaman8Solution :: AST
lambdaman8Solution =
    Concat
        "solve lambdaman8 "
        spin
