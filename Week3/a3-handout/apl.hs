module Main (main) where

import APL.Parser (parseAPL)
import APL.AST (Exp(..))
import System.IO (hPutStrLn, stdout)

main :: IO ()
main = do
  --let test1 = parseAPL "" "x y z"
  --let test2 = parseAPL "" "x/y"
  --let test3 = parseAPL "" "x if x then y else z"
  --let test4 = parseAPL "" "x*y**z"
  --let test5 = parseAPL "" "x+y==y+x"

  --let test1 = parseAPL "" "x ** y ** z"
  --let test2 = parseAPL "" "a * b / c * d"
  --let test3 = parseAPL "" "a + b - c + d"
  --let test4 = parseAPL "" "a == b + c * d"
  --let test5 = parseAPL "" "x+y==y+x"
  -- let test6 = parseAPL "" "put x y"
  let test7 = parseAPL "" "get x + y"
  let test10 = parseAPL "" "print \"Sum:\" x + y"
  --let test8 = parseAPL "" "getx"
  --let test9 = parseAPL "" "print \"foo\" x"

  --putStrLn $ "> parseAPL \"\" \"x y z\""
  --printResult test1
  --printResult test2
  --printResult test3
  --printResult test4
  --printResult test5
  -- printResult test6
  printResult test7
  printResult test10
  --printResult test8
  -- printResult test9

printResult :: Show a => Either String a -> IO ()
printResult (Left err) = putStrLn $ "Left " ++ show err
printResult (Right expr) = putStrLn $ "Right " ++ show expr

-- Right Mul (Div (Mul (Var "a") (Var "b")) (Var "c")) (Var "d")
-- Right Mul (Div (Mul (Var "a") (Var "b")) (Var "c")) (Var "d")

-- Right Add (Sub (Add (Var "a") (Var "b")) (Var "c")) (Var "d")
-- Right Add (Sub (Add (Var "a") (Var "b")) (Var "c")) (Var "d")
