module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

newtype EvalM a = EvalM (Either Error a)

instance Functor EvalM where
  fmap _ (EvalM (Left e))  = EvalM $ Left e
  fmap f (EvalM (Right x)) = EvalM $ Right $ f x

instance Applicative EvalM where
  pure x = EvalM $ Right x
  EvalM (Left e) <*> _ = EvalM $ Left e
  _ <*> EvalM (Left e) = EvalM $ Left e
  EvalM (Right f) <*> EvalM (Right x) = EvalM $ Right $ f x

instance Monad EvalM where
  EvalM x >>= f = EvalM $ case x of
    Left err -> Left err
    Right x' ->
      let EvalM y = f x'
        in y

failure :: String -> EvalM a
failure s = EvalM $ Left s

catch :: EvalM a -> EvalM a -> EvalM a


runEval :: EvalM a -> Either Error a
runEval (EvalM x) = x

eval :: Env -> Exp -> EvalM Val
eval _ (CstInt i) = pure $ ValInt i
eval _ (CstBool b) = pure $ ValBool b
eval env (Var v) = do
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Variable " ++ v ++ " not found"
eval env (Add e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x, y) of
    (ValInt x', ValInt y') -> pure $ ValInt $ x' + y'
    _ -> failure "Type error in addition"
eval env (Sub e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x, y) of
    (ValInt x', ValInt y') -> pure $ ValInt $ x' - y'
    _ -> failure "Type error in subtraction"
eval env (Mul e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x, y) of
    (ValInt x', ValInt y') -> pure $ ValInt $ x' * y'
    _ -> failure "Type error in multiplication"
eval env (Div e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x, y) of
    (ValInt x', ValInt y')
      | y' == 0 -> failure "Division by zero"
      | otherwise -> pure $ ValInt $ x' `div` y'
    _ -> failure "Type error in division"
eval env (Pow e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x, y) of
    (ValInt x', ValInt y')
      | y' < 0 -> failure "Negative exponent"
      | otherwise -> pure $ ValInt $ x' ^ y'
    _ -> failure "Type error in exponentiation"
eval env (Eql e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x, y) of
    (ValInt x', ValInt y') -> pure $ ValBool $ x' == y'
    _ -> failure "Type error in equality comparison"
eval env (If cond e1 e2) = do
  x <- eval env cond
  case x of
    ValBool True -> eval env e1
    ValBool False -> eval env e2
    _ -> failure "Type error in if condition"
eval env (Let v e1 e2) = do
  x <- eval env e1
  eval (envExtend v x env) e2
eval env (Lambda v e) = 
  pure $ ValFun env v e
eval env (Apply e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x, y) of
    (ValFun env' v e', _) -> eval (envExtend v y env') e'
    _ -> failure "Type error in function application"
eval env (TryCatch e1 e2) =
  eval env e1 `catch` eval env e2