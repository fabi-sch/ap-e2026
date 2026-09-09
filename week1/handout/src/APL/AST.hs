module APL.AST
  (
    Exp (..),
    VName,
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Var VName
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Let VName Exp Exp
  deriving (Eq, Show)
