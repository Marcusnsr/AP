module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "123 " $ CstInt 123,
          parserTestFail "123f",
          parserTest "true" $ CstBool True,
          parserTest "false" $ CstBool False
        ],
      testGroup
        "Basic operators"
        [ parserTest "x+y" $ Add (Var "x") (Var "y"),
          parserTest "x-y" $ Sub (Var "x") (Var "y"),
          parserTest "x*y" $ Mul (Var "x") (Var "y"),
          parserTest "x/y" $ Div (Var "x") (Var "y")
        ],
      testGroup
        "Operator priority"
        [ parserTest "x+y+z" $ Add (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y-z" $ Sub (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z" $ Add (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x*y*z" $ Mul (Mul (Var "x") (Var "y")) (Var "z"),
          parserTest "x/y/z" $ Div (Div (Var "x") (Var "y")) (Var "z")
        ],
      testGroup
        "Conditional expressions"
        [ parserTest "if x then y else z" $ If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then y else if x then y else z" $
            If (Var "x") (Var "y") $
              If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then (if x then y else z) else z" $
            If (Var "x") (If (Var "x") (Var "y") (Var "z")) (Var "z"),
          parserTest "1 + if x then y else z" $
            Add (CstInt 1) (If (Var "x") (Var "y") (Var "z"))
        ],
      testGroup
        "Lexing edge cases"
        [ parserTest "2 " $ CstInt 2,
          parserTest " 2" $ CstInt 2
        ],
      testGroup
        "Equality and Power Operators"
        [ parserTest "x**y" $ Pow (Var "x") (Var "y"),
          parserTest "x**y**z" $ Pow (Var "x") (Pow (Var "y") (Var "z")),
          parserTest "x==y" $ Eql (Var "x") (Var "y"),
          parserTest "x==y==z" $ Eql (Eql (Var "x") (Var "y")) (Var "z"),
          parserTest "x*y**z" $ Mul (Var "x") (Pow (Var "y") (Var "z")),
          parserTest "x+y==y+x" $ Eql (Add (Var "x") (Var "y")) (Add (Var "y") (Var "x")),
          parserTest "x**y*z" $ Mul (Pow (Var "x") (Var "y")) (Var "z"),
          parserTestFail "x**",
          parserTestFail "x==",
          parserTestFail "x***y",
          parserTestFail "x==*y",
          parserTestFail "*x==y",
          parserTestFail "x**y**",
          parserTestFail "x==y==",
          parserTestFail "==x==y"
        ],
      testGroup
        "Function application"
        [ parserTest "x y z" $ Apply (Apply (Var "x") (Var "y")) (Var "z"),
          parserTest "x(y z)" $ Apply (Var "x") (Apply (Var "y") (Var "z")),
          parserTest "g(1 2)" $ Apply (Var "g") (Apply (CstInt 1) (CstInt 2)),
          parserTest "true false" $ Apply (CstBool True) (CstBool False),
          parserTestFail "x if x then y else z"  -- Expecting a parse error
        ],
      testGroup
        "Put construct"
        [ parserTest "put x y" $ KvPut (Var "x") (Var "y"),
          parserTest "put (x+1) (y*2)" $
            KvPut (Add (Var "x") (CstInt 1)) (Mul (Var "y") (CstInt 2)),
          parserTest "put (x + y) (z * w)" $
            KvPut (Add (Var "x") (Var "y")) (Mul (Var "z") (Var "w")),
          parserTest "put ((x + 1) * z) (y - 2)" $
            KvPut (Mul (Add (Var "x") (CstInt 1)) (Var "z")) (Sub (Var "y") (CstInt 2)),
          parserTestFail "put x",
          parserTestFail "put"
        ],
      testGroup
        "Get construct"
        [ parserTest "get x" $ KvGet (Var "x"),
          parserTest "get (x + y)" $ KvGet (Add (Var "x") (Var "y")),
          parserTest "get (x * y)" $ KvGet (Mul (Var "x") (Var "y")),
          parserTest "get ((x + 2) * y)" $ KvGet (Mul (Add (Var "x") (CstInt 2)) (Var "y")),
          parserTestFail "get",
          parserTestFail "get x y",
          parserTestFail "get x + y"
        ],
      testGroup
        "Print construct"
        [ parserTest "print \"Value:\" x" $ Print "Value:" (Var "x"),
          parserTest "print \"Sum:\" x + y" $
            Print "Sum:" (Add (Var "x") (Var "y")),
          parserTest "print \"Product:\" x * y" $
            Print "Product:" (Mul (Var "x") (Var "y")),
          parserTestFail "print \"Missing expr\"",
          parserTestFail "print x"
        ]
    ]
