{-# OPTIONS_GHC -Wall #-}

module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- Consider this example when you have added the necessary constructors.
-- The Y combinator in a form suitable for strict evaluation.
yComb :: Exp
yComb =
  Lambda "f" $
    Apply
      (Lambda "g" (Apply (Var "g") (Var "g")))
      ( Lambda
          "g"
          ( Apply
              (Var "f")
              (Lambda "a" (Apply (Apply (Var "g") (Var "g")) (Var "a")))
          )
      )

fact :: Exp
fact =
  Apply yComb $
    Lambda "rec" $
      Lambda "n" $
        If
          (Eql (Var "n") (CstInt 0))
          (CstInt 1)
          (Mul (Var "n") (Apply (Var "rec") (Sub (Var "n") (CstInt 1))))

tests :: TestTree
tests = testGroup "Evaluation"
  [ testCase "Add" $
      eval envEmpty (Add (CstInt 2) (CstInt 5))
        @?= Right (ValInt 7),
    --
    testCase "Add (wrong type)" $
      eval envEmpty (Add (CstInt 2) (CstBool True))
        @?= Left "Non-integer operand",
    --
    testCase "Sub" $
      eval envEmpty (Sub (CstInt 2) (CstInt 5))
        @?= Right (ValInt (-3)),
    --
    testCase "Div" $
      eval envEmpty (Div (CstInt 7) (CstInt 3))
        @?= Right (ValInt 2),
    --
    testCase "Div0" $
      eval envEmpty (Div (CstInt 7) (CstInt 0))
        @?= Left "Division by zero",
    --
    testCase "Pow" $
      eval envEmpty (Pow (CstInt 2) (CstInt 3))
        @?= Right (ValInt 8),
    --
    testCase "Pow0" $
      eval envEmpty (Pow (CstInt 2) (CstInt 0))
        @?= Right (ValInt 1),
    --
    testCase "Pow negative" $
      eval envEmpty (Pow (CstInt 2) (CstInt (-1)))
        @?= Left "Negative exponent",
    --
    testCase "Eql (false)" $
      eval envEmpty (Eql (CstInt 2) (CstInt 3))
        @?= Right (ValBool False),
    --
    testCase "Eql (true)" $
      eval envEmpty (Eql (CstInt 2) (CstInt 2))
        @?= Right (ValBool True),
    --
    testCase "If" $
      eval envEmpty (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
        @?= Right (ValInt 2),
    --
    testCase "Let" $
      eval envEmpty (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
        @?= Right (ValInt 5),
    --
    testCase "Let (shadowing)" $
      eval
        envEmpty
        ( Let
            "x"
            (Add (CstInt 2) (CstInt 3))
            (Let "x" (CstBool True) (Var "x"))
        )
        @?= Right (ValBool True),
    --
    testCase "Lambda succ" $
      eval
        envEmpty
        (
          Let "x" (CstInt 2) $ (Lambda "y" (Sub (Var "x") (Var "y")))
        )
        @?= Right (ValFun [("x", ValInt 2)] "y" (Sub (Var "x") (Var "y"))),
    --
    testCase "Apply succ" $
      eval 
        envEmpty
        (
          Apply (Let "x" ( CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 3)
        )
        @?=
        Right (ValInt 5),
    --
    testCase "Apply fail" $
      eval 
        envEmpty
        (
          Apply (CstInt 3) (CstInt 3)
        )
        @?=
        Left "Non-function applied",
    --
    testCase "TryCatch case 1" $
      eval 
        envEmpty
        (
          TryCatch (CstInt 0) (CstInt 1)
        )
        @?=
        Right (ValInt 0),
    --
    testCase "TryCatch case 2" $
      eval 
        envEmpty
        (
          TryCatch (Div (CstInt 1) (CstInt 0)) (CstInt 1)
        )
        @?=
        Right (ValInt 1),
    --
    testCase "TryCatch both fail" $
      eval 
        envEmpty
        (
          TryCatch (Div (CstInt 1) (CstInt 0)) (Div (CstInt 1) (CstInt 0))
        )
        @?=
        Left "Both expressions failed",
    --
    testCase "Factorial of 0" $
      eval envEmpty (Apply fact (CstInt 0))
        @?= Right (ValInt 1),
    --
    testCase "Factorial of 1" $
      eval envEmpty (Apply fact (CstInt 1))
        @?= Right (ValInt 1),
    --
    testCase "Factorial of 5" $
      eval envEmpty (Apply fact (CstInt 5))
        @?= Right (ValInt 120),
    --
    testCase "Factorial of 10" $
      eval envEmpty (Apply fact (CstInt 10))
        @?= Right (ValInt 3628800)
    --
    {- testCase "Infinite recursion" $
      eval envEmpty (Apply (Lambda "x" (Apply (Var "x") (Var "x"))) (Lambda "x" (Apply (Var "x") (Var "x"))))
        @?= Left "Stack overflow" -- or a suitable error message for infinite recursion
    -}
    --
  ]