module Lambdaman8 where

import qualified Data.Text as T

import AST
import StringBitCoding (makeRecursion)

double :: AST
double = 0 --> Concat input input
    where
    input = Var 0

repeatStr :: AST
repeatStr = makeRecursion repeater
    where
        self = Var 0
        char = Var 1
        count = Var 2

        repeater =
            0 --> -- self
            1 --> -- char
            2 --> -- count
            (If
                (count =~ 1)
                (char)
                (Concat char ((self $$ char) $$ (count - 1))))

repeatDL :: AST
repeatDL =
    0 --> -- count
    (Concat ((repeatStr $$ "DD") $$ count) ((repeatStr $$ "LL") $$ count))
    where
    count = Var 0

repeatUR :: AST
repeatUR =
    0 --> -- count
    (Concat ((repeatStr $$ "UU") $$ count) ((repeatStr $$ "RR") $$ count))
    where
    count = Var 0

repeatLine :: AST
repeatLine =
    0 --> -- iteration
    (If
        (Equals
            1
            (Mod iteration 2))
        (repeatDL $$ iteration)
        (repeatUR $$ iteration))
    where
    iteration = Var 0

times :: AST
times = makeRecursion go
    where
    self = Var 0
    action = Var 1
    iterations_left = Var 2

    go =
        0 --> -- self
        1 --> -- action
        2 --> -- iterations_left
        (If
            (Equals iterations_left 1)
            (action)
            (Concat
              ((self $$ action) $$ (iterations_left-1))
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

lambdaman8Solution :: AST
lambdaman8Solution =
    (Concat
        "solve lambdaman8 "
        ((times $$ loop) $$ 25))
