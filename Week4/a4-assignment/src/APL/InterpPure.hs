module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' r s (Free (TryCatchOp m1 m2)) =  -- Added this case
      let (ps1, res1) = runEval' r s m1
       in case res1 of -- Troels kører også "in case" med et single space indent ¯\_(ツ)_/¯ 
            Left _ ->
              let (ps2, res2) = runEval' r s m2
               in (ps1 ++ ps2, res2) -- Troels kører også "in case" med et single space indent ¯\_(ツ)_/¯ 
            Right x -> (ps1, Right x)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e) -- Samme som før!
