module APL.Eval
  (
    Val (..),
    eval,
    envEmpty,
  )
where

import APL.AST (Exp (..), VName)

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

type Error = String

type Env = [(VName, Val)]

-- | Empty environment, which contains no variable bindings.
envEmpty :: Env
envEmpty = []

-- | Extend an environment with a new variable binding,
-- producing a new environment.
envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val):env

-- | Look up a variable name in the provided environment.
-- Returns Nothing if the variable is not in the environment.
envLookup :: VName -> Env -> Maybe Val
envLookup _ [] = Nothing
envLookup x ((k, v) : rest)
  | x == k = Just v
  | otherwise = envLookup x rest

evalBinOp :: Env -> Exp -> Exp -> Either Error (Integer, Integer)
evalBinOp env e1 e2 =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt a), Right (ValInt b)) -> Right (a, b)

eval :: Env -> Exp -> Either Error Val
eval env (CstInt a) = Right $ ValInt a
eval env (CstBool a) = Right $ ValBool a
eval env (Add e1 e2) = do
  (a, b) <- evalBinOp env e1 e2
  Right $ ValInt $ a + b
eval env (Sub e1 e2) = do
  (a, b) <- evalBinOp env e1 e2
  Right $ ValInt $ a - b
eval env (Mul e1 e2) = do
  (a, b) <- evalBinOp env e1 e2
  Right $ ValInt $ a * b
eval env (Div e1 e2) = do
  (a, b) <- evalBinOp env e1 e2
  if b == 0
    then Left "Division by zero"
    else Right $ ValInt $ a `div` b
eval env (Pow e1 e2) = do
  (a, b) <- evalBinOp env e1 e2
  if b < 0
    then Left "Negative exponent"
    else Right $ ValInt $ a ^ b
eval env (Eql e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt a), Right (ValInt b)) -> Right $ ValBool $ a == b
    (Right (ValBool a), Right (ValBool b)) -> Right $ ValBool $ a == b
    (Right _, Right _) -> Left "Cannot compare values of different types"
eval env (If cond e1 e2) =
  case eval env cond of
    Left err -> Left err
    Right (ValBool True) -> eval env e1
    Right (ValBool False) -> eval env e2
    (Right (ValInt _)) -> Left "Non-boolean conditional"
eval env (Var v) =
  case envLookup v env of
    Nothing -> Left "Variable not in environment"
    Just val -> Right val
eval env (Let v e1 e2) =
  case eval env e1 of
    Left err -> Left err
    Right val -> eval (envExtend v val env) e2
