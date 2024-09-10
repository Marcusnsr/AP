module APL.Eval
  (
    Val(..),
    eval,
    envEmpty,
    envExtend,
    envLookup
  )
where

import APL.AST (Exp(..), VName)

type Error = String

type Env = [(VName, Val)]

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

eval :: Env -> Exp -> Either Error Val
eval _ (CstInt x) = Right $ ValInt x
eval _ (CstBool y) = Right $ ValBool y
eval env (Add e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt (x + y)
eval env (Sub e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt (x - y)
eval env (Mul e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt (x * y)
eval env (Div e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (_, Right (ValInt 0)) -> Left "Division by zero"
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt (x `div` y)
eval env (Pow e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (_, Right (ValInt n)) | n < 0 -> Left "Negative power"
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt (x ^ y)
eval env (Eql e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool (x == y)
    (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool (x == y)
    (Right _, Right _) -> Left "Cannot compare different types"
eval env (If cond e1 e2) =
  case eval env cond of
    Left err -> Left err
    Right (ValBool True) -> eval env e1
    Right (ValBool False) -> eval env e2
    Right _ -> Left "Condition is not a boolean"
eval env (Var v) =
  case envLookup v env of
    Just val -> Right val
    Nothing -> Left $ "Unbound variable: " ++ v
eval env (Let v e1 e2) =
  case eval env e1 of
    Left err -> Left err
    Right val -> eval (envExtend v val env) e2

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend vn vl e = (vn, vl) : e

envLookup :: VName -> Env -> Maybe Val
envLookup vn e = lookup vn e