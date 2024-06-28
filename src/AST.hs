module AST (AST(..), VarNo, evalAst) where

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
    deriving (Show, Eq)

--- Returns either the reduced AST or the error message explaining what went wrong.
evalAst :: AST -> Either T.Text AST
evalAst input@(Boolean _) = Right input
evalAst input@(Number _) = Right input
evalAst input@(Str _) = Right input
evalAst (Negate (Number input)) = Right $ Number (negate input)
evalAst (Negate _) = Left "Non-integer input for unary negation"
evalAst (Not (Boolean input)) = Right $ Boolean (not input)
evalAst (Not _) = Left "Non-boolean input for \"not\""
-- evalAst (StrToInt AST)
-- evalAst (IntToStr AST)
-- evalAst (Add AST AST)
-- evalAst (Sub AST AST)
-- evalAst (Mult AST AST)
-- evalAst (Div AST AST)
-- evalAst (Mod AST AST)
-- evalAst (Lt AST AST)
-- evalAst (Gt AST AST)
-- evalAst (Equals AST AST)
-- evalAst (Or AST AST)
-- evalAst (And AST AST)
-- evalAst (Concat AST AST)
-- evalAst (Take AST AST)
-- evalAst (Drop AST AST)
-- evalAst (Apply AST AST)
-- evalAst (If AST AST AST)
-- evalAst (Var VarNo)
-- evalAst (Lambda VarNo AST)
evalAst _ = undefined
