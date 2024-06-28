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
evalAst ast = do
    reduced <- evalAstStep ast
    if reduced == ast
        then Right ast
        else evalAst reduced

evalAstStep :: AST -> Either T.Text AST
evalAstStep input@(Boolean _) = Right input
evalAstStep input@(Number _) = Right input
evalAstStep input@(Str _) = Right input
evalAstStep (Negate input) = do
  reduced <- evalAst input
  case reduced of
    (Number number) -> Right $ Number (negate number)
    _ -> Left "Non-integer input for unary negation"
evalAstStep (Not input) = do
  reduced <- evalAst input
  case reduced of
    (Boolean bool) -> Right $ Boolean (not bool)
    _ -> Left "Non-boolean input for \"not\""
evalAstStep (StrToInt input) = do
  reduced <- evalAst input
  case reduced of
    (Str str) ->
      case parseNumber (textToGalaxy str) of
        Just number -> Right $ Number number
        Nothing -> Left $ "Failed to parse " <> str <> " as number"
    _ -> Left "Non-string input to str-to-int"
evalAstStep (IntToStr input) = do
  reduced <- evalAst input
  case reduced of
    (Number number) -> Right $ Str $ textFromGalaxy $ numberToGalaxy number
    _ -> Left "Non-integer input to int-to-str"
evalAstStep (Add lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Number a, Number b) -> Right $ Number (a+b)
    _ -> Left "Non-integer inputs to addition"
evalAstStep (Sub lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Number a, Number b) -> Right $ Number (a-b)
    _ -> Left "Non-integer inputs to subtraction"
evalAstStep (Mult lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Number a, Number b) -> Right $ Number (a*b)
    _ -> Left "Non-integer inputs to multiplication"
evalAstStep (Div lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Number a, Number b) -> Right $ Number (a `quot` b)
    _ -> Left "Non-integer inputs to division"
evalAstStep (Mod lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Number a, Number b) -> Right $ Number (a `rem` b)
    _ -> Left "Non-integer inputs to modulo"
evalAstStep (Lt lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Number a, Number b) -> Right $ Boolean (a < b)
    _ -> Left "Non-integer inputs to less-than"
evalAstStep (Gt lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Number a, Number b) -> Right $ Boolean (a > b)
    _ -> Left "Non-integer inputs to greater-than"
evalAstStep (Equals lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Number a, Number b) -> Right $ Boolean (a == b)
    (Boolean a, Boolean b) -> Right $ Boolean (a == b)
    (Str a, Str b) -> Right $ Boolean (a == b)
    _ -> Left "Arguments to equals have mismatching types or they weren't reduced completely"
evalAstStep (Or lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Boolean a, Boolean b) -> Right $ Boolean (a || b)
    _ -> Left "Non-boolean inputs to \"or\""
evalAstStep (And lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Boolean a, Boolean b) -> Right $ Boolean (a && b)
    _ -> Left "Non-boolean inputs to \"and\""
evalAstStep (Concat lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Str a, Str b) -> Right $ Str (T.concat [a, b])
    _ -> Left "Non-string inputs to concat"
evalAstStep (Take lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Number a, Str b) -> Right $ Str (T.take (fromIntegral a) b)
    _ -> Left "Invalid argument types to \"take\""
evalAstStep (Drop lhs rhs) = do
  lhs' <- evalAst lhs
  rhs' <- evalAst rhs
  case (lhs', rhs') of
    (Number a, Str b) -> Right $ Str (T.drop (fromIntegral a) b)
    _ -> Left "Invalid argument types to \"drop\""
-- evalAst (Apply AST AST)
-- evalAst (If AST AST AST)
-- evalAst (Var VarNo)
-- evalAst (Lambda VarNo AST)
evalAstStep _ = undefined
