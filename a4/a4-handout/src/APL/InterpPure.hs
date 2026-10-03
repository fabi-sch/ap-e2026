module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)
    runEval' r s (Free (TryCatchOp m1 m2 v)) = do
      x <- runEval' r s m1
      case x of
        (Right x') -> runEval' r s (v x')
        (Left _) -> do
          y <- runEval' r s m2
          case y of
            (Right y') -> runEval' r s (v y')
            (Left err) -> pure (Left err)
    runEval' r s (Free (KvGetOp v h)) = case lookup v s of
      Just v' -> runEval' r s $ h v'
      Nothing -> ([], Left $ "Invalid key: " ++ show v)
    runEval' r s (Free (KvPutOp v1 v2 m)) = runEval' r s' m
      where
        s' = (v1, v2) : ((filter $ (/= v1) . fst) s)
