module AST (AST (..), VarNo, evalAst) where

import qualified Data.Text as T
import Control.Monad.State

import Lib (parseNumber, printNumber)
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
    deriving (Eq, Show)

--- Returns either the reduced AST or the error message explaining what went wrong.
evalAst :: AST -> Either T.Text AST
evalAst = evalAstIter . renumberVariables

evalAstIter :: AST -> Either T.Text AST
evalAstIter ast = do
    reduced <- evalAstStep ast
    if reduced == ast
        then Right ast
        else evalAstIter reduced

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
      case (T.uncons . textToGalaxy $ str) >>= parseNumber . snd of
        Just number -> Right $ Number number
        Nothing -> Left $ "Failed to parse " <> str <> " as number"
    _ -> Left "Non-string input to str-to-int"
evalAstStep (IntToStr input) = do
  reduced <- evalAst input
  case reduced of
    (Number number) -> Right $ Str $ textFromGalaxy $ printNumber number
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
evalAstStep (Mult (Number 0) _) = pure $ Number 0
evalAstStep (Mult _ (Number 0)) = pure $ Number 0
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
evalAstStep (If cond lhs rhs) = do
  cond' <- evalAst cond
  case cond' of
    Boolean bool -> Right $ if bool then lhs else rhs
    _ -> Left "Condition doesn't evaluate to a boolean"
evalAstStep input@(Var _) = Right input
evalAstStep input@(Lambda _ _) = Right input
evalAstStep (Apply fn arg) = do
  fn' <- evalAst fn
  case fn' of
    Lambda lambdaVar body ->
      let body' = replaceVar lambdaVar arg body
      in Right body'
    _ -> Left "First argument of apply didn't evaluate to a lambda"
  where
    replaceVar var value (Var varno) =
      if varno == var
        then value
        else Var varno
    replaceVar var value (Negate ast) = Negate $ replaceVar var value ast
    replaceVar var value (Not ast) = Not $ replaceVar var value ast
    replaceVar var value (StrToInt ast) = StrToInt $ replaceVar var value ast
    replaceVar var value (IntToStr ast) = IntToStr $ replaceVar var value ast
    replaceVar var value (Add lhs rhs) = Add (replaceVar var value lhs) (replaceVar var value rhs)
    replaceVar var value (Sub lhs rhs) = Sub (replaceVar var value lhs) (replaceVar var value rhs)
    replaceVar var value (Mult lhs rhs) = Mult (replaceVar var value lhs) (replaceVar var value rhs)
    replaceVar var value (Div lhs rhs) = Div (replaceVar var value lhs) (replaceVar var value rhs)
    replaceVar var value (Mod lhs rhs) = Mod (replaceVar var value lhs) (replaceVar var value rhs)
    replaceVar var value (Lt lhs rhs) = Lt (replaceVar var value lhs) (replaceVar var value rhs)
    replaceVar var value (Gt lhs rhs) = Gt (replaceVar var value lhs) (replaceVar var value rhs)
    replaceVar var value (Equals lhs rhs) = Equals (replaceVar var value lhs) (replaceVar var value rhs)
    replaceVar var value (Or lhs rhs) = Or (replaceVar var value lhs) (replaceVar var value rhs)
    replaceVar var value (And lhs rhs) = And (replaceVar var value lhs) (replaceVar var value rhs)
    replaceVar var value (Concat lhs rhs) = Concat (replaceVar var value lhs) (replaceVar var value rhs)
    replaceVar var value (Take lhs rhs) = Take (replaceVar var value lhs) (replaceVar var value rhs)
    replaceVar var value (Drop lhs rhs) = Drop (replaceVar var value lhs) (replaceVar var value rhs)
    replaceVar var value (Apply lhs rhs) = Apply (replaceVar var value lhs) (replaceVar var value rhs)
    replaceVar var value (If cond lhs rhs) = If (replaceVar var value cond) (replaceVar var value lhs) (replaceVar var value rhs)
    replaceVar var value (Lambda varno body) = Lambda varno (replaceVar var value body)
    replaceVar _ _ expr = expr

getMaxVarNo :: AST -> Integer
getMaxVarNo (Boolean _) = 0
getMaxVarNo (Number _) = 0
getMaxVarNo (Str _) = 0
getMaxVarNo (Negate ast) = getMaxVarNo ast
getMaxVarNo (Not ast) = getMaxVarNo ast
getMaxVarNo (StrToInt ast) = getMaxVarNo ast
getMaxVarNo (IntToStr ast) = getMaxVarNo ast
getMaxVarNo (Add lhs rhs) = getMaxVarNo lhs `max` getMaxVarNo rhs
getMaxVarNo (Sub lhs rhs) = getMaxVarNo lhs `max` getMaxVarNo rhs
getMaxVarNo (Mult lhs rhs) = getMaxVarNo lhs `max` getMaxVarNo rhs
getMaxVarNo (Div lhs rhs) = getMaxVarNo lhs `max` getMaxVarNo rhs
getMaxVarNo (Mod lhs rhs) = getMaxVarNo lhs `max` getMaxVarNo rhs
getMaxVarNo (Lt lhs rhs) = getMaxVarNo lhs `max` getMaxVarNo rhs
getMaxVarNo (Gt lhs rhs) = getMaxVarNo lhs `max` getMaxVarNo rhs
getMaxVarNo (Equals lhs rhs) = getMaxVarNo lhs `max` getMaxVarNo rhs
getMaxVarNo (Or lhs rhs) = getMaxVarNo lhs `max` getMaxVarNo rhs
getMaxVarNo (And lhs rhs) = getMaxVarNo lhs `max` getMaxVarNo rhs
getMaxVarNo (Concat lhs rhs) = getMaxVarNo lhs `max` getMaxVarNo rhs
getMaxVarNo (Take lhs rhs) = getMaxVarNo lhs `max` getMaxVarNo rhs
getMaxVarNo (Drop lhs rhs) = getMaxVarNo lhs `max` getMaxVarNo rhs
getMaxVarNo (Apply lhs rhs) = getMaxVarNo lhs `max` getMaxVarNo rhs
getMaxVarNo (If cond lhs rhs) = getMaxVarNo cond `max` getMaxVarNo lhs `max` getMaxVarNo rhs
getMaxVarNo (Var varno) = varno
getMaxVarNo (Lambda varno ast) = varno `max` getMaxVarNo ast

data RenumberingState = RenumberingState {
    nextVarNo :: VarNo
  , shadowingStack :: [(VarNo, VarNo)] -- (old, new)
}

initialState :: VarNo -> RenumberingState
initialState varNo =
  RenumberingState {
      nextVarNo = varNo + 1
    , shadowingStack = []
    }

type RenumberingStateM a = State RenumberingState a

--- Add an old varno to the stack and get the corresponding new varno.
addVarNo :: VarNo -> RenumberingStateM VarNo
addVarNo varNo = do
    newVarNo <- gets nextVarNo
    modify' (\s -> s { nextVarNo = nextVarNo s + 1, shadowingStack = (varNo, newVarNo):(shadowingStack s) } )
    pure newVarNo

--- Translate old varno into new varno.
lookupVarNo :: VarNo -> RenumberingStateM VarNo
lookupVarNo oldVarNo = do
  stack <- gets shadowingStack
  pure $ go stack
  where
  go [] = error "lookupVarNo: reached the bottom of the stack but didn't find your variable!"
  go ((varNo, newVarNo):xs) =
    if varNo == oldVarNo
      then newVarNo
      else go xs

--- Forget the topmost shadowing.
pop :: RenumberingStateM ()
pop = modify $ \s -> s { shadowingStack = tail $ shadowingStack s }

renumberVariablesM :: AST -> RenumberingStateM AST
renumberVariablesM (Negate ast) = Negate <$> renumberVariablesM ast
renumberVariablesM (Not ast) = Not <$> renumberVariablesM ast
renumberVariablesM (StrToInt ast) = StrToInt <$> renumberVariablesM ast
renumberVariablesM (IntToStr ast) = IntToStr <$> renumberVariablesM ast
renumberVariablesM (Add lhs rhs) = Add <$> renumberVariablesM lhs <*> renumberVariablesM rhs
renumberVariablesM (Sub lhs rhs) = Sub <$> renumberVariablesM lhs <*> renumberVariablesM rhs
renumberVariablesM (Mult lhs rhs) = Mult <$> renumberVariablesM lhs <*> renumberVariablesM rhs
renumberVariablesM (Div lhs rhs) = Div <$> renumberVariablesM lhs <*> renumberVariablesM rhs
renumberVariablesM (Mod lhs rhs) = Mod <$> renumberVariablesM lhs <*> renumberVariablesM rhs
renumberVariablesM (Lt lhs rhs) = Lt <$> renumberVariablesM lhs <*> renumberVariablesM rhs
renumberVariablesM (Gt lhs rhs) = Gt <$> renumberVariablesM lhs <*> renumberVariablesM rhs
renumberVariablesM (Equals lhs rhs) = Equals <$> renumberVariablesM lhs <*> renumberVariablesM rhs
renumberVariablesM (Or lhs rhs) = Or <$> renumberVariablesM lhs <*> renumberVariablesM rhs
renumberVariablesM (And lhs rhs) = And <$> renumberVariablesM lhs <*> renumberVariablesM rhs
renumberVariablesM (Concat lhs rhs) = Concat <$> renumberVariablesM lhs <*> renumberVariablesM rhs
renumberVariablesM (Take lhs rhs) = Take <$> renumberVariablesM lhs <*> renumberVariablesM rhs
renumberVariablesM (Drop lhs rhs) = Drop <$> renumberVariablesM lhs <*> renumberVariablesM rhs
renumberVariablesM (Apply lhs rhs) = Apply <$> renumberVariablesM lhs <*> renumberVariablesM rhs
renumberVariablesM (If cond lhs rhs) = If <$> renumberVariablesM cond <*> renumberVariablesM lhs <*> renumberVariablesM rhs
renumberVariablesM (Var varNo) = Var <$> lookupVarNo varNo
renumberVariablesM (Lambda varNo ast) = do
    newVarNo <- addVarNo varNo
    result <- (Lambda newVarNo) <$> renumberVariablesM ast
    pop
    pure result
renumberVariablesM ast = pure ast

{-
* иметь счётчик, начинающийся со 100500 (> максимального номера переменной)
* вести таблицу символов — стек пар [старый номер, новый номер]
* когда заходим в (Lambda n: x), то:
 берём новый номер N из счётчика
 записываем в стек пару (n, N)
** пока обрабатываем x, везде где видим n заменяем его на *самое верхнее соответствие из стека*
* когда выходим из Lambda, то верхний элемент стека отбрасываем
-}
renumberVariables :: AST -> AST
renumberVariables ast = evalState (renumberVariablesM ast) (initialState (getMaxVarNo ast))
