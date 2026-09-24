module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

newtype EvalM a = EvalM (Env -> Either Error a)

instance Functor EvalM where
  -- fmap :: (a -> b) -> EvalM a -> EvalM b
  fmap f (EvalM x) =
    EvalM $ \env ->
      case x env of
        Right v -> Right $ f v
        Left err -> Left err

instance Applicative EvalM where
  -- pure :: a -> EvalM a
  pure x = EvalM $ \_ -> Right x

  -- (<*>) :: EvalM (a -> b) -> EvalM a -> EvalM b
  EvalM f <*> EvalM x =
    EvalM $ \env ->
      case f env of
        Left err -> Left err
        Right g ->
          case x env of
            Left err -> Left err
            Right v -> Right (g v)

instance Monad EvalM where
  -- (>>=) :: EvalM a -> (a -> EvalM b) -> EvalM b
  EvalM x >>= f =
    EvalM $ \env ->
      case x env of
        Left err -> Left err
        Right v -> case f v of
          EvalM g -> g env

runEval :: EvalM a -> Either Error a
runEval (EvalM x) = x envEmpty

askEnv :: EvalM Env
askEnv = EvalM (\env -> Right env)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env ->
  m (f env)

failure :: String -> EvalM a
failure s = EvalM $ \_ -> Left s

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) =
  EvalM $ \env ->
    case m1 env of
      Left _ -> m2 env
      Right x -> Right x

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool x) = pure $ ValBool x
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure $ x
    Nothing -> failure ("Unknown variable: " ++ v)
eval (Add e1 e2) = do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValInt x', ValInt y') -> pure $ ValInt (x' + y')
    (_, _) -> failure "Non-integer operators"
eval (Sub e1 e2) =
  eval e1 >>= \x ->
  eval e2 >>= \y ->
  case (x, y) of
    (ValInt x', ValInt y') -> pure $ ValInt (x' - y')
    (_, _) -> failure "Non-integer operators"
eval (Mul e1 e2) =
  eval e1 >>= \x ->
  eval e2 >>= \y ->
  case (x, y) of
    (ValInt x', ValInt y') -> pure $ ValInt (x' * y')
    (_, _) -> failure "Non-integer operators"
eval (Div e1 e2) =
  eval e1 >>= \x ->
  eval e2 >>= \y ->
  case (x, y) of
    (ValInt x', ValInt y')
      | y' == 0 -> failure "Zero-division"
      | otherwise -> pure $ ValInt (x' `div` y')
    (_, _) -> failure "Non-integer operators"
eval (Pow e1 e2) =
  eval e1 >>= \x ->
  eval e2 >>= \y ->
  case (x, y) of
    (ValInt x', ValInt y')
      | y' < 0 -> failure "Negative-pow"
      | otherwise -> pure $ ValInt (x' ^ y')
    (_, _) -> failure "Non-integer operators"
eval (Eql e1 e2) =
  eval e1 >>= \x ->
  eval e2 >>= \y ->
  case (x, y) of
    (ValBool x', ValBool y') -> pure $ ValBool (x' == y')
    (ValInt x', ValInt y') -> pure $ ValBool (x' == y')
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) =
  eval cond >>= \c ->
  case c of
    (ValBool True) -> eval e1
    (ValBool False) -> eval e2
    _ -> failure "Non-boolean conditional"
eval (Let var e1 e2) =
  eval e1 >>= \x ->
  localEnv (envExtend var x) (eval e2)
eval (ForLoop (p, initial) (i, bound) body) =
  eval initial >>= \initVal ->
  case initVal of
    (ValInt accStart) ->
      eval bound >>= \boundVal ->
        case boundVal of
          (ValInt n) -> go accStart 0
            where
              go acc idx
                | idx >= n = pure $ ValInt acc
                | otherwise =
                  localEnv (envExtend p (ValInt acc)) ((localEnv (envExtend i (ValInt idx))) (eval body)) >>= \r ->
                  case r of
                    (ValInt acc') -> go acc' (idx + 1)
                    _ -> failure "Non-integral body"
          _ -> failure "Invalid bound"
    _ -> failure "Invalid initial"
eval (Lambda var e) =
  askEnv >>= \env ->
  pure (ValFun env var e)
eval (Apply e1 e2) =
  eval e1 >>= \func ->
  eval e2 >>= \arg ->
  case func of
    (ValFun env var e) -> localEnv (\_ -> envExtend var arg env) (eval e)
    _ -> failure "Not a function"
eval (TryCatch e1 e2) =
  catch (eval e1) (eval e2)
