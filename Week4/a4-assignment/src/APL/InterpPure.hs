module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)  -- If computation is pure, return the value
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r  -- Handle ReadOp by passing the current environment
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s  -- Handle StateGetOp by passing the current state
    runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m  -- Handle StatePutOp by updating the state
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
      in (p : ps, res)  -- Handle PrintOp by adding the printed message to the list of strings
    runEval' r s (Free (TryCatchOp m1 m2)) =  -- Added this case
      let (ps1, res1) = runEval' r s m1
       in case res1 of -- Troels kører også "in case" med et single space indent ¯\_(ツ)_/¯ 
            Left _ ->
              let (ps2, res2) = runEval' r s m2
               in (ps1 ++ ps2, res2) -- Troels kører også "in case" med et single space indent ¯\_(ツ)_/¯ 
            Right x -> (ps1, Right x)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e) -- Samme som før!  -- Handle ErrorOp by returning an error message

    -- Handle KvGetOp
    runEval' r s (Free (KvGetOp key k)) =
      case lookup key s of
        Just val -> runEval' r s (k val)  -- If key is found in the state, pass the value to the continuation
        Nothing  -> ([], Left $ "Key not found: " ++ show key)  -- If key not found, return an error

    -- Handle KvPutOp
    runEval' r s (Free (KvPutOp key val m)) =
      let s' = (key, val) : filter ((/= key) . fst) s  -- Update state by replacing or adding the (key, value) pair
      in runEval' r s' m  -- Continue interpreting with the updated state