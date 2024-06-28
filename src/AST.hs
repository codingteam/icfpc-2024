module AST (AST, evalAst) where

import qualified Data.Text as T

type VarNo = Integer

data AST =
      Boolean Bool
    | Number Integer
    | Str T.Text
    | Negate AST
    | Not AST
    | StrToInt AST
    | IntToStr AST
    | Add AST AST
    | Sub AST AST
    | Mult AST AST
    | Div AST AST
    | Mod AST AST
    | Lt AST AST
    | Gt AST AST
    | Equals AST AST
    | Or AST AST
    | And AST AST
    | Concat AST AST
    | Take AST AST
    | Drop AST AST
    | Apply AST AST
    | If AST AST AST
    | Var VarNo
    | Lambda VarNo AST

evalAst :: AST -> AST
evalAst = undefined
