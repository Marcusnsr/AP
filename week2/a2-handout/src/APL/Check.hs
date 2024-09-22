module APL.Check (checkExp, Error) where

import APL.AST (Exp(..), VName)
import Control.Monad (liftM, ap)

type Error = String
type Env = [VName]

newtype CheckM a = CheckM { runCheckM :: Env -> Maybe a }

instance Functor CheckM where
    fmap = liftM

instance Applicative CheckM where
    pure x = CheckM $ \_ -> Just x
    (<*>) = ap

instance Monad CheckM where
    CheckM m >>= f = CheckM $ \env ->
        case m env of
            Nothing -> Nothing
            Just x  -> runCheckM (f x) env

throwError :: CheckM a
throwError = CheckM $ \_ -> Nothing

askEnv :: CheckM Env
askEnv = CheckM $ \env -> Just env

extendEnv :: VName -> CheckM a -> CheckM a
extendEnv v (CheckM m) = CheckM $ \env -> m (v : env)

checkVar :: VName -> CheckM ()
checkVar v = do
    env <- askEnv
    if v `elem` env
        then return ()
        else throwError

check :: Exp -> CheckM ()
check (CstInt _) = return ()
check (CstBool _) = return ()
check (Var v) = checkVar v
check (Add e1 e2) = check e1 >> check e2
check (Sub e1 e2) = check e1 >> check e2
check (Mul e1 e2) = check e1 >> check e2
check (Div e1 e2) = check e1 >> check e2
check (Pow e1 e2) = check e1 >> check e2
check (Eql e1 e2) = check e1 >> check e2
check (If e1 e2 e3) = check e1 >> check e2 >> check e3
check (Let v e body) = check e >> extendEnv v (check body)
check (Lambda v body) = extendEnv v (check body)
check (Apply e1 e2) = check e1 >> check e2
check (TryCatch e1 e2) = check e1 >> check e2
check (Print _ e) = check e
check (KvPut e1 e2) = check e1 >> check e2
check (KvGet e) = check e

checkExp :: Exp -> Maybe Error
checkExp exp =
    case runCheckM (check exp) [] of
        Nothing -> Just "Variable not in scope"
        Just _  -> Nothing