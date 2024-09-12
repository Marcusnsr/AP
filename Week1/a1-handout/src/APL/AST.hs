{-# OPTIONS_GHC -Wall #-}

module APL.AST
  ( VName,
    Exp (..),
    printExp,
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  deriving (Eq, Show)

-- Task 4: Implement a pretty printer for expressions (recursive function)
printExp :: Exp -> String
printExp (CstInt x) = show x
printExp (CstBool b) = show b
printExp (Add e1 e2) = "(" ++ printExp e1 ++ " + " ++ printExp e2 ++ ")"
printExp (Sub e1 e2) = "(" ++ printExp e1 ++ " - " ++ printExp e2 ++ ")"
printExp (Mul e1 e2) = "(" ++ printExp e1 ++ " * " ++ printExp e2 ++ ")"
printExp (Div e1 e2) = "(" ++ printExp e1 ++ " / " ++ printExp e2 ++ ")"
printExp (Pow e1 e2) = "(" ++ printExp e1 ++ " ** " ++ printExp e2 ++ ")"
printExp (Eql e1 e2) = "(" ++ printExp e1 ++ " == " ++ printExp e2 ++ ")"
printExp (If e1 e2 e3) = "(if " ++ printExp e1 ++ " then " ++ printExp e2 ++ " else " ++ printExp e3 ++ ")"
printExp (Var v) = "(" ++ v ++ ")"
printExp (Let v e1 e2) = "(let " ++ v ++ " = " ++ printExp e1 ++ " in " ++ printExp e2 ++ ")"
printExp (Lambda v e) = "(\\" ++ v ++ " -> " ++ printExp e ++ ")"
printExp (Apply e1 e2) = "(" ++ printExp e1 ++ " " ++ printExp e2 ++ ")"
printExp (TryCatch e1 e2) = "(try " ++ printExp e1 ++ " catch " ++ printExp e2 ++ ")"
-- Task 4 ends here