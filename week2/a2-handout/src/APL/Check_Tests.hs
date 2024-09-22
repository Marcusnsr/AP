module APL.Check_Tests (tests) where

import APL.AST (Exp(..))
import APL.Check (checkExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- Assert that the provided expression should pass the type checker.
testPos :: Exp -> TestTree
testPos e =
  testCase (show e) $
    checkExp e @?= Nothing

-- Assert that the provided expression should fail the type checker.
testNeg :: Exp -> TestTree
testNeg e =
  testCase (show e) $
    case checkExp e of
      Nothing -> assertFailure "expected error"
      Just _ -> pure ()

tests :: TestTree
tests =
  testGroup
    "Checking"
    [ testPos (CstInt 2)  -- Constant should pass
    , testPos (Lambda "x" (Var "x"))  -- Variable bound by lambda
    , testPos (Let "x" (CstInt 3) (Var "x"))  -- Variable bound by let

    -- Test variables out of scope
    , testNeg (Var "x")  -- Unbound variable should fail
    , testNeg (Apply (Lambda "x" (Var "x")) (Var "y"))  -- 'y' not bound in scope

    -- Test shadowing
    , testPos (Let "x" (CstInt 3) (Let "x" (CstBool True) (Var "x")))  -- Inner 'x' should shadow outer

    -- Test nested Let
    , testPos (Let "x" (CstInt 2) (Let "y" (Add (Var "x") (CstInt 3)) (Var "y")))
    , testNeg (Let "x" (CstInt 2) (Add (Var "y") (CstInt 3)))  -- 'y' is not in scope

    -- Test Let and Apply
    , testPos (Let "f" (Lambda "x" (Add (Var "x") (CstInt 1))) (Apply (Var "f") (CstInt 2)))

    -- Test failed Let
    , testNeg (Let "f" (Lambda "x" (Add (Var "x") (Var "y"))) (Apply (Var "f") (CstInt 2)))  -- 'y' is not bound
    ]
