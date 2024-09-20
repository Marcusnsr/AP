module APL.Eval
  ( Val (..),
    Env,
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

type State = ([String], [(Val, Val)])  -- (prints, key-value pairs)

newtype EvalM a = EvalM (Env -> State -> Either Error (a, State))

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_env state -> Right (x, state)
  (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env state ->
    case x env state of
      Left err -> Left err
      Right (x', state') ->
        let EvalM y = f x'
         in y env state'

askEnv :: EvalM Env
askEnv = EvalM $ \env state -> Right (env, state)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env state -> m (f env) state

failure :: String -> EvalM a
failure s = EvalM $ \_env _state -> Left s

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env state ->
  case m1 env state of
    Left _ -> m2 env state
    Right (x, state') -> Right (x, state')
  
runEval :: EvalM a -> ([String], Either Error a)
runEval (EvalM m) = 
  let initialEnv = envEmpty
      initialState = ([], [])  -- (prints, store)
  in case m initialEnv initialState of
      Left err -> (fst initialState, Left err)  -- Captures initial prints
      Right (result, (prints, _store)) -> (prints, Right result)

evalPrint :: String -> EvalM ()
evalPrint str = EvalM $ \_env (prints, store) -> Right ((), (prints ++ [str], store))

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut key val = EvalM $ \_env (prints, store) ->
  let newStore = (key, val) : filter ((/= key) . fst) store
  in Right ((), (prints, newStore))

evalKvGet :: Val -> EvalM Val
evalKvGet key = EvalM $ \_env (prints, store) ->
  case lookup key store of
    Just val -> Right (val, (prints, store))
    Nothing  -> Left "KvGet: key not found"


evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then failure "Negative exponent"
        else pure $ x ^ y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Non-boolean conditional."
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2
eval (Print s e) = do
  v <- eval e
  let pVal = case v of
        ValInt i  -> show i
        ValBool b -> show b
        ValFun _ _ _ -> "#<fun>"
  evalPrint (s ++ ": " ++ pVal)
  pure v
eval (KvPut k_exp v_exp) = do
  k <- eval k_exp
  v <- eval v_exp
  evalKvPut k v
  pure v
eval (KvGet k_exp) = do
  k <- eval k_exp
  evalKvGet k
