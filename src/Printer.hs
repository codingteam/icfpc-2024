module Printer where

import qualified Data.Text as T

import AST
import Lib
import Strings

astToGalaxy :: AST -> T.Text
astToGalaxy (Boolean True) = "T"
astToGalaxy (Boolean False) = "F"
astToGalaxy (Number n) = "I" <> printNumber n
astToGalaxy (Str txt) = textToGalaxy txt
astToGalaxy (Negate x) = "U- " <> astToGalaxy x
astToGalaxy (Not x) = "U! " <> astToGalaxy x
astToGalaxy (StrToInt x) = "U# " <> astToGalaxy x
astToGalaxy (IntToStr x) = "U$ " <> astToGalaxy x
astToGalaxy (Add x y) = "B+ " <> astToGalaxy x <> " " <> astToGalaxy y
astToGalaxy (Sub x y) = "B- " <> astToGalaxy x <> " " <> astToGalaxy y
astToGalaxy (Mult x y) = "B* " <> astToGalaxy x <> " " <> astToGalaxy y
astToGalaxy (Div x y) = "B/ " <> astToGalaxy x <> " " <> astToGalaxy y
astToGalaxy (Mod x y) = "B% " <> astToGalaxy x <> " " <> astToGalaxy y
astToGalaxy (Lt x y) = "B< " <> astToGalaxy x <> " " <> astToGalaxy y
astToGalaxy (Gt x y) = "B> " <> astToGalaxy x <> " " <> astToGalaxy y
astToGalaxy (Equals x y) = "B= " <> astToGalaxy x <> " " <> astToGalaxy y
astToGalaxy (Or x y) = "B| " <> astToGalaxy x <> " " <> astToGalaxy y
astToGalaxy (And x y) = "B& " <> astToGalaxy x <> " " <> astToGalaxy y
astToGalaxy (Concat x y) = "B. " <> astToGalaxy x <> " " <> astToGalaxy y
astToGalaxy (Take x y) = "BT " <> astToGalaxy x <> " " <> astToGalaxy y
astToGalaxy (Drop x y) = "BD " <> astToGalaxy x <> " " <> astToGalaxy y
astToGalaxy (Apply f x) = "B$ " <> astToGalaxy f <> " " <> astToGalaxy x
astToGalaxy (If cond true false) = "? " <> astToGalaxy cond <> " " <> astToGalaxy true <> " " <> astToGalaxy false
astToGalaxy (Var n) = "v" <> printNumber n
astToGalaxy (Lambda n x) = "L" <> printNumber n <> " " <> astToGalaxy x
