module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, Val (..), eval, runEval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalTests :: TestTree
evalTests =
  testGroup
    "EValuation"
    [ testCase "Add" $
        snd (eval' (Add (CstInt 2) (CstInt 5)))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        snd (eval' (Add (CstInt 2) (CstBool True)))
          @?= Left "Non-integer operand",
      --
      testCase "Sub" $
        snd (eval' (Sub (CstInt 2) (CstInt 5)))
          @?= Right (ValInt (-3)),
      --
      testCase "Div" $
        snd (eval' (Div (CstInt 7) (CstInt 3)))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        snd (eval' (Div (CstInt 7) (CstInt 0)))
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        snd (eval' (Pow (CstInt 2) (CstInt 3)))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        snd (eval' (Pow (CstInt 2) (CstInt 0)))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        snd (eval' (Pow (CstInt 2) (CstInt (-1))))
          @?= Left "Negative exponent",
      --
      testCase "Eql (false)" $
        snd (eval' (Eql (CstInt 2) (CstInt 3)))
          @?= Right (ValBool False),
      --
      testCase "Eql (true)" $
        snd (eval' (Eql (CstInt 2) (CstInt 2)))
          @?= Right (ValBool True),
      --
      testCase "If" $
        snd (eval' (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0))))
          @?= Right (ValInt 2),
      --
      testCase "Let" $
        snd (eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x")))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        snd (eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          ))
          @?= Right (ValBool True),
      --
      testCase "Lambda/Apply" $
        snd (eval'
          (Apply (Lambda "x" (Mul (Var "x") (Var "x"))) (CstInt 4)))
          @?= Right (ValInt 16),
      --
      testCase "TryCatch" $
        snd (eval'
          (TryCatch (Div (CstInt 7) (CstInt 0)) (CstBool True)))
          @?= Right (ValBool True)
    ]

tests :: TestTree
tests = 
  testGroup 
    "APL" 
    [
      evalTests
    ]