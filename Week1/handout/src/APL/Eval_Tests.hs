module APL.Eval_Tests (tests) where

import APL.AST (Exp(..))
import APL.Eval (Val(..), eval, envEmpty, envExtend, envLookup)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [
      testGroup
        "Arithmetic tests"
        [
          testCase "CstInt" $
            eval envEmpty (CstInt 42) @?= Right (ValInt 42),
          testCase "DivByZero" $
            eval envEmpty (Div (CstInt 1) (CstInt 0)) @?= Left "Division by zero",
          testCase "NegPow" $
            eval envEmpty (Pow (CstInt 2) (CstInt (-1))) @?= Left "Negative power"
        ],
      testGroup
        "If then else and equal"
        [
          testCase "Equality" $
            eval envEmpty (Eql (CstInt 42) (CstInt 42)) @?= Right (ValBool True),
          testCase "Not" $
            eval envEmpty (Eql (CstInt 42) (CstInt 1)) @?= Right (ValBool False),
          testCase "If then else" $
            eval envEmpty (If (Eql (CstInt 42) (CstInt 42)) (CstInt 1) (CstInt 2)) @?= Right (ValInt 1),
          testCase "If then else false" $
            eval envEmpty (If (Eql (CstInt 42) (CstInt 43)) (CstInt 1) (CstInt 2)) @?= Right (ValInt 2),
          testCase "different types" $
            eval envEmpty (If (Eql (CstInt 42) (CstBool True)) (CstInt 1) (CstInt 2)) @?= Left "Cannot compare different types"
        ],
      testGroup
        "Env tests"
        [
          testCase "Empty env" $
            envEmpty @?= [],
          testCase "Extending environments" $
            envExtend "first" (ValInt 1) envEmpty @?= [("first", ValInt 1)],
          testCase "LookUp" $
            envLookup "x" envEmpty @?= Nothing,
          testCase "NonEmptyList" $
            envLookup "x" [("x", ValInt 1), ("x2", ValInt 1), ("x3", ValInt 2)] @?= Just (ValInt 1),
          testCase "Eval with environment" $
            eval (envExtend "x" (ValInt 5) envEmpty) (Var "x") @?= Right (ValInt 5)
        ],
      testGroup
        "Var and Let tests"
        [
          testCase "Unbound variable" $
            eval envEmpty (Var "x") @?= Left "Unbound variable: x",
          testCase "Bound variable" $
            eval (envExtend "x" (ValInt 5) envEmpty) (Var "x") @?= Right (ValInt 5),
          testCase "Simple Let" $
            eval envEmpty (Let "x" (CstInt 5) (Add (Var "x") (CstInt 3))) @?= Right (ValInt 8),
          testCase "Let with failure" $
            eval envEmpty (Let "x" (Div (CstInt 1) (CstInt 0)) (Var "x")) @?= Left "Division by zero"
        ]
    ]
