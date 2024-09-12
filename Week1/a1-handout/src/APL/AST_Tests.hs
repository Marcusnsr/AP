module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "Prettyprinting"
    [   testCase "Pretty print int" $
            printExp (CstInt 2)
                @?= "2",
        --
        testCase "Pretty print add" $
            printExp (Add (CstInt 2) (CstInt 3))
                @?= "(2 + 3)",
        --
        testCase "Pretty print sub" $
            printExp (Sub (CstInt 2) (CstInt 3))
                @?= "(2 - 3)",
        --
        testCase "Pretty print mul" $
            printExp (Mul (CstInt 2) (CstInt 3))
                @?= "(2 * 3)",
        --
        testCase "Pretty print div" $
            printExp (Div (CstInt 2) (CstInt 3))
                @?= "(2 / 3)",
        --
        testCase "Pretty print pow" $
            printExp (Pow (CstInt 2) (CstInt 3))
                @?= "(2 ** 3)",
        --
        testCase "Pretty print eql" $
            printExp (Eql (CstInt 2) (CstInt 3))
                @?= "(2 == 3)",
        --
        testCase "Pretty print if" $
            printExp (If (CstBool True) (CstInt 2) (CstInt 3))
                @?= "(if True then 2 else 3)",
        --
        testCase "Pretty print var" $
            printExp (Var "Hello")
                @?= "Hello",
        --
        testCase "Pretty print let" $
            printExp (Let "x" (CstInt 2) (Var "x"))
                @?= "(let x = 2 in x)",
        --
        testCase "Pretty print lambda" $
            printExp (Lambda "x" (Add (Var "x") (CstInt 2)))
                @?= "(\\x -> (x + 2))",
        --
        testCase "Apply lambda to constant" $
            printExp (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstInt 5))
                @?= "((\\x -> (x + 1)) 5)",
        --
        testCase "Apply variable to variable" $
            printExp (Apply (Var "f") (Var "x"))
                @?= "(f x)",
        --
        testCase "Nested Apply" $
            printExp (Apply (Apply (Var "f") (Var "x")) (Var "y"))
                @?= "((f x) y)",
        --
        testCase "Apply with complex function" $
            printExp (Apply (Let "f" (Lambda "x" (Add (Var "x") (CstInt 1))) (Var "f")) (CstInt 5))
                @?= "((let f = (\\x -> (x + 1)) in f) 5)",
        --
        testCase "Apply with complex argument" $
            printExp (Apply (Var "f") (If (CstBool True) (CstInt 1) (CstInt 2)))
                @?= "(f (if True then 1 else 2))",
        --
        testCase "Apply with arithmetic expression as argument" $
            printExp (Apply (Var "f") (Add (Mul (CstInt 2) (CstInt 3)) (CstInt 4)))
                @?= "(f ((2 * 3) + 4))",
        --
        testCase "Multiple nested Applies" $
            printExp (Apply (Apply (Apply (Var "f") (Var "x")) (Var "y")) (Var "z"))
                @?= "(((f x) y) z)",
        --
        testCase "Apply with lambda and complex argument" $
            printExp (Apply (Lambda "x" (Mul (Var "x") (Var "x"))) (Add (CstInt 2) (CstInt 3)))
                @?= "((\\x -> (x * x)) (2 + 3))",
        --
        testCase "Apply with TryCatch as function" $
            printExp (Apply (TryCatch (Lambda "x" (Var "x")) (Lambda "y" (CstInt 0))) (CstInt 5))
                @?= "((try (\\x -> x) catch (\\y -> 0)) 5)",
        --
        testCase "Apply with TryCatch as argument" $
            printExp (Apply (Var "f") (TryCatch (Div (CstInt 1) (CstInt 0)) (CstInt 1)))
                @?= "(f (try (1 / 0) catch 1))",
        --
        testCase "Apply with Pow as function" $
            printExp (Apply (Pow (Var "f") (CstInt 2)) (Var "x"))
                @?= "((f ** 2) x)",
        --
        testCase "Apply with Eql in argument" $
            printExp (Apply (Var "f") (Eql (Var "x") (Var "y")))
                @?= "(f (x == y))",
        --
        testCase "Pretty print tryCatch" $
            printExp (TryCatch (CstInt 1) (CstInt 2))
                @?= "(try 1 catch 2)"
        --
    ]
