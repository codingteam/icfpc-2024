module AST (
    AST (..),
    VarNo,
    evalAst,
    ($$), (-->),
    (=~), (>~), (<~),
    (><)
    ) where

import qualified Data.Text as T
import Control.Monad.State
import Data.Functor.Identity
import Data.String

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

($$) :: AST -> AST -> AST
($$) = Apply
infixr 0 $$

(-->) :: VarNo -> AST -> AST
(-->) = Lambda
infixr 0 -->

(=~) :: AST -> AST -> AST
(=~) = Equals
infix 4 =~

(>~) :: AST -> AST -> AST
(>~) = Gt
infix 4 >~

(<~) :: AST -> AST -> AST
(<~) = Lt
infix 4 <~

(><) :: AST -> AST -> AST
(><) = Concat
infixr 5 ><

instance Num AST where
    (+) = Add
    (-) = Sub
    (*) = Mult
    abs = undefined
    signum = undefined
    negate = Negate
    fromInteger = Number

instance IsString AST where
    fromString = Str . T.pack

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
    replaceVar var value ast =
        runIdentity $
            traverseAst
                (\innerAst -> pure $ replaceVar var value innerAst)
                (\varNo -> if varNo == var then pure value else pure $ Var varNo)
                (\varNo body -> pure $ Lambda varNo (replaceVar var value body))
                ast

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
renumberVariablesM ast =
    traverseAst
        renumberVariablesM
        (\varNo -> Var <$> lookupVarNo varNo)
        (\varNo body -> do
            newVarNo <- addVarNo varNo
            result <- (Lambda newVarNo) <$> renumberVariablesM body
            pop
            pure result)
        ast

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

traverseAst :: Applicative m => (AST -> m AST) -> (VarNo -> m AST) -> (VarNo -> AST -> m AST) -> AST -> m AST
traverseAst processAst _processVar _processLambda (Negate ast) = Negate <$> processAst ast
traverseAst processAst _processVar _processLambda (Not ast) = Not <$> processAst ast
traverseAst processAst _processVar _processLambda (StrToInt ast) = StrToInt <$> processAst ast
traverseAst processAst _processVar _processLambda (IntToStr ast) = IntToStr <$> processAst ast
traverseAst processAst _processVar _processLambda (Add lhs rhs) = Add <$> processAst lhs <*> processAst rhs
traverseAst processAst _processVar _processLambda (Sub lhs rhs) = Sub <$> processAst lhs <*> processAst rhs
traverseAst processAst _processVar _processLambda (Mult lhs rhs) = Mult <$> processAst lhs <*> processAst rhs
traverseAst processAst _processVar _processLambda (Div lhs rhs) = Div <$> processAst lhs <*> processAst rhs
traverseAst processAst _processVar _processLambda (Mod lhs rhs) = Mod <$> processAst lhs <*> processAst rhs
traverseAst processAst _processVar _processLambda (Lt lhs rhs) = Lt <$> processAst lhs <*> processAst rhs
traverseAst processAst _processVar _processLambda (Gt lhs rhs) = Gt <$> processAst lhs <*> processAst rhs
traverseAst processAst _processVar _processLambda (Equals lhs rhs) = Equals <$> processAst lhs <*> processAst rhs
traverseAst processAst _processVar _processLambda (Or lhs rhs) = Or <$> processAst lhs <*> processAst rhs
traverseAst processAst _processVar _processLambda (And lhs rhs) = And <$> processAst lhs <*> processAst rhs
traverseAst processAst _processVar _processLambda (Concat lhs rhs) = Concat <$> processAst lhs <*> processAst rhs
traverseAst processAst _processVar _processLambda (Take lhs rhs) = Take <$> processAst lhs <*> processAst rhs
traverseAst processAst _processVar _processLambda (Drop lhs rhs) = Drop <$> processAst lhs <*> processAst rhs
traverseAst processAst _processVar _processLambda (Apply lhs rhs) = Apply <$> processAst lhs <*> processAst rhs
traverseAst processAst _processVar _processLambda (If cond lhs rhs) = If <$> processAst cond <*> processAst lhs <*> processAst rhs
traverseAst _processAst processVar _processLambda (Var varNo) = processVar varNo
traverseAst _processAst _processVar processLambda (Lambda varNo ast) = processLambda varNo ast
traverseAst _processAst _processVar _processLambda ast = pure ast
