module Scheme (
    toScheme
) where

import qualified Data.Text as T

import AST

toScheme :: AST -> T.Text
toScheme (Boolean True) = "true"
toScheme (Boolean False) = "false"
toScheme (Number n) = T.pack (show n)
toScheme (Str str) = T.pack (show str)
toScheme (Negate x) = "(- " <> toScheme x <> ")"
toScheme (Not x) = "(not " <> toScheme x <> ")"
toScheme (StrToInt x) = "(str-to-int " <> toScheme x <> ")"
toScheme (IntToStr x) = "(int-to-str " <> toScheme x <> ")"
toScheme (Add x y) = "(+ " <> toScheme x <> " " <> toScheme y <> ")"
toScheme (Sub x y) = "(- " <> toScheme x <> " " <> toScheme y <> ")"
toScheme (Mult x y) = "(* " <> toScheme x <> " " <> toScheme y <> ")"
toScheme (Div x y) = "(quotient " <> toScheme x <> " " <> toScheme y <> ")"
toScheme (Mod x y) = "(modulo " <> toScheme x <> " " <> toScheme y <> ")"
toScheme (Lt x y) = "(< " <> toScheme x <> " " <> toScheme y <> ")"
toScheme (Gt x y) = "(> " <> toScheme x <> " " <> toScheme y <> ")"
toScheme (Equals x y) = "(= " <> toScheme x <> " " <> toScheme y <> ")"
toScheme (Or x y) = "(or " <> toScheme x <> " " <> toScheme y <> ")"
toScheme (And x y) = "(and " <> toScheme x <> " " <> toScheme y <> ")"
toScheme (Concat x y) = "(string-append " <> toScheme x <> " " <> toScheme y <> ")"
toScheme (Take x y) = "(substring " <> toScheme y <> " 0 " <> toScheme x <> ")"
toScheme (Drop x y) = "(substring " <> toScheme y <> " " <> toScheme x <> ")"
toScheme (Apply f x) = "(" <> toScheme f <> " " <> toScheme x <> ")"
toScheme (If cond true false) = "(if " <> toScheme cond <> " " <> toScheme true <> " " <> toScheme false <> ")"
toScheme (Var n) = "v" <> T.pack (show n)
toScheme (Lambda v body) = "(lambda (v" <> T.pack (show v) <> ") " <> toScheme body <> ")"

