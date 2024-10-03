module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests, getputTests, tryCatchTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "State" $
        runEval
          ( do
              putState [(ValInt 0, ValInt 1)]
              modifyState $ map (\(key, _) -> (key, ValInt 5))
              getState
          )
          @?= ([], Right [(ValInt 0, ValInt 5)]),
      --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero")
    ]

getputTests :: TestTree
getputTests =
  testGroup
    "KvGetOp and KvPutOp Tests"
    [ testCase "KvPut and KvGet" $
        runEval
          ( do
              evalKvPut (ValInt 42) (ValInt 100)
              evalKvGet (ValInt 42)
          )
          @?= ([], Right (ValInt 100)),  -- The value 100 should be retrieved
      --
      testCase "KvGet missing key" $
        runEval
          ( do
              evalKvGet (ValInt 999)  -- Key 999 has not been put in the state
          )
          @?= ([], Left "Key not found: ValInt 999"),  -- Should return an error message
      --
      testCase "KvPut overwrite existing key" $
        runEval
          ( do
              evalKvPut (ValInt 42) (ValInt 100)
              evalKvPut (ValInt 42) (ValInt 200)  -- Overwrite the value for key 42
              evalKvGet (ValInt 42)
          )
          @?= ([], Right (ValInt 200)),  -- The updated value 200 should be retrieved
      --
      testCase "KvPut multiple keys" $
        runEval
          ( do
              evalKvPut (ValInt 1) (ValInt 10)
              evalKvPut (ValInt 2) (ValInt 20)
              val1 <- evalKvGet (ValInt 1)
              val2 <- evalKvGet (ValInt 2)
              return (val1, val2)
          )
          @?= ([], Right (ValInt 10, ValInt 20)),  -- Ensure both values are correct
      --
      testCase "KvPut and KvGet order of operations" $
        runEval
          ( do
              evalKvPut (ValInt 1) (ValInt 10)
              val1 <- evalKvGet (ValInt 1)
              evalKvPut (ValInt 2) (ValInt 20)
              val2 <- evalKvGet (ValInt 2)
              evalKvPut (ValInt 1) (ValInt 100)  -- Overwrite key 1
              val1Updated <- evalKvGet (ValInt 1)
              return (val1, val2, val1Updated)
          )
          @?= ([], Right (ValInt 10, ValInt 20, ValInt 100)),  -- Verify the correct sequence
      --
      testCase "KvGet from empty state" $
        runEval
          ( do
              evalKvGet (ValInt 1)  -- No key has been added yet
          )
          @?= ([], Left "Key not found: ValInt 1"),
      --
      testCase "KvPut and KvGet with non-integer key" $
        runEval
          ( do
              evalKvPut (ValBool True) (ValInt 100)
              evalKvGet (ValBool True)
          )
          @?= ([], Right (ValInt 100)),  -- Using a boolean as a key
      --
      testCase "Database integrity after multiple operations" $
        runEval
          ( do
              evalKvPut (ValInt 42) (ValInt 100)
              evalKvPut (ValInt 43) (ValBool True)
              evalKvPut (ValInt 42) (ValInt 200)  -- Overwrite key 42
              val1 <- evalKvGet (ValInt 42)  -- Should return 200
              val2 <- evalKvGet (ValInt 43)  -- Should return True
              return (val1, val2)
          )
          @?= ([], Right (ValInt 200, ValBool True)),  -- Ensure both values are correct
      --
      testCase "Missing key test" $ do
        (_, res) <- captureIO ["ValInt 1"] $  -- Simulate user input
          runEvalIO $ Free $ KvGetOp (ValInt 0) $ \val -> pure val
        res @?= Right (ValInt 1)  -- Ensure the returned value matches expected input
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ())
        -- NOTE: This test will give a runtime error unless you replace the
        -- version of `eval` in `APL.Eval` with a complete version that supports
        -- `Print`-expressions. Uncomment at your own risk.
        -- testCase "print 2" $ do
        --    (out, res) <-
        --      captureIO [] $
        --        evalIO' $
        --          Print "This is also 1" $
        --            Print "This is 1" $
        --              CstInt 1
        --    (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1)
    ]

-- NEW TESTS FOR TryCatchOp
tryCatchTests :: TestTree
tryCatchTests =
  testGroup
    "TryCatchOp Tests"
    [ testCase "TryCatchOp with failure" $
        let test1 :: ([String], Either Error String)
            test1 = runEval $ Free $ TryCatchOp (failure "Oh no!") (pure "Success!")
        in test1 @?= ([], Right "Success!"),
      --
      testCase "TryCatch with division by zero" $
        let divZero :: Exp
            divZero = CstInt 1 `Div` CstInt 0
            test2 :: ([String], Either Error Val)
            test2 = runEval $ eval $ TryCatch (CstInt 5) divZero
        in test2 @?= ([], Right (ValInt 5)),
      --
      testCase "TryCatch with bad equality check" $ do
        let divZero :: Exp
            divZero = CstInt 1 `Div` CstInt 0
            badEql :: Exp
            badEql = CstInt 0 `Eql` CstBool True
            test3 :: IO (Either Error Val)
            test3 = runEvalIO $ eval $ TryCatch badEql divZero
        res <- test3
        res @?= Left "Division by zero"
    ]