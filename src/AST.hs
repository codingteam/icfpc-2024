module AST (AST(..), VarNo, evalAst) where

import qualified Data.Text as T

import Lib (parseNumber, numberToGalaxy)
import Strings (textToGalaxy, textFromGalaxy)

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
evalAst (Negate input) = do
  reduced <- evalAst input
  case reduced of
    (Number number) -> Right $ Number (negate number)
    _ -> Left "Non-integer input for unary negation"
evalAst (Not input) = do
  reduced <- evalAst input
  case reduced of
    (Boolean bool) -> Right $ Boolean (not bool)
    _ -> Left "Non-boolean input for \"not\""
evalAst (StrToInt input) = do
  reduced <- evalAst input
  case reduced of
    (Str str) ->
      case parseNumber (textToGalaxy str) of
        Just number -> Right $ Number number
        Nothing -> Left $ "Failed to parse " <> str <> " as number"
    _ -> Left "Non-string input to str-to-int"
evalAst (IntToStr input) = do
  reduced <- evalAst input
  case reduced of
    (Number number) -> Right $ Str $ textFromGalaxy $ numberToGalaxy number
    _ -> Left "Non-integer input to int-to-str"
evalAst (Add lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Number a, Number b) -> Right $ Number (a+b)
    _ -> Left "Non-integer inputs to addition"
evalAst (Sub lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Number a, Number b) -> Right $ Number (a-b)
    _ -> Left "Non-integer inputs to subtraction"
evalAst (Mult lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Number a, Number b) -> Right $ Number (a*b)
    _ -> Left "Non-integer inputs to multiplication"
evalAst (Div lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Number a, Number b) -> Right $ Number (a `quot` b)
    _ -> Left "Non-integer inputs to division"
evalAst (Mod lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Number a, Number b) -> Right $ Number (a `rem` b)
    _ -> Left "Non-integer inputs to modulo"
evalAst (Lt lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Number a, Number b) -> Right $ Boolean (a < b)
    _ -> Left "Non-integer inputs to less-than"
evalAst (Gt lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Number a, Number b) -> Right $ Boolean (a > b)
    _ -> Left "Non-integer inputs to greater-than"
evalAst (Equals lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Number a, Number b) -> Right $ Boolean (a == b)
    (Boolean a, Boolean b) -> Right $ Boolean (a == b)
    (Str a, Str b) -> Right $ Boolean (a == b)
    _ -> Left "Arguments to equals have mismatching types or they weren't reduced completely"
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
