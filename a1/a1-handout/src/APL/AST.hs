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
  | ForLoop (VName, Exp) (VName, Exp) Exp
  deriving (Eq, Show)

printExp :: Exp -> String
printExp (CstInt x) = show x
printExp (CstBool x) = if x then "true" else "false"
printExp (Var x) = x
printExp (Add e1 e2) = "(" ++ printExp e1 ++ " + " ++ printExp e2 ++ ")"
printExp (Sub e1 e2) = "(" ++ printExp e1 ++ " - " ++ printExp e2 ++ ")"
printExp (Mul e1 e2) = "(" ++ printExp e1 ++ " * " ++ printExp e2 ++ ")"
printExp (Div e1 e2) = "(" ++ printExp e1 ++ " / " ++ printExp e2 ++ ")"
printExp (Pow e1 e2) = "(" ++ printExp e1 ++ " ** " ++ printExp e2 ++ ")"
printExp (Eql e1 e2) = "(" ++ printExp e1 ++ " == " ++ printExp e2 ++ ")"
printExp (If e1 e2 e3) = "(if " ++ printExp e1 ++ " then " ++ printExp e2 ++ " else " ++ printExp e3 ++ ")"
printExp (Let v1 e1 e2) = "(let " ++ v1 ++ " = " ++ printExp e1 ++ " in " ++ printExp e2 ++ ")"
printExp (ForLoop (p, initial) (i, bound) body) = "(loop " ++ p ++ " = " ++ printExp initial ++ " for " ++ i ++ " < " ++ printExp bound ++ " do " ++ printExp body ++ ")"
