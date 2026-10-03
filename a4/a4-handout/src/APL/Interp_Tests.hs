module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

divZero :: Exp
divZero = CstInt 1 `Div` CstInt 0

badEql :: Exp
badEql = CstInt 0 `Eql` CstBool True

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),
      --
      testCase "TryCatchOp (m1 fails)" $
        runEval (Free $ TryCatchOp (failure "Oh no!") (pure $ ValInt 1) pure)
          @?= ([], Right (ValInt 1)),
      --
      testCase "TryCatchOp (m1 succeeds)" $
        runEval (Free $ TryCatchOp (pure $ ValInt 0) (pure $ ValInt 1) pure)
          @?= ([], Right (ValInt 0)),
      --
      testCase "TryCatch (no error)" $
        eval' (TryCatch (CstInt 5) divZero)
          @?= ([], Right (ValInt 5)),
      --
      testCase "TryCatch (error caught)" $
        eval' (TryCatch divZero (CstInt 5))
          @?= ([], Right (ValInt 5)),
      --
      testCase "TryCatch (both fail)" $
        eval' (TryCatch badEql divZero)
          @?= ([], Left "Division by zero"),
      --
      testCase "catch does not run m2 if m1 succeeds" $
        runEval (catch (pure $ ValInt 1) (evalPrint "m2" >> pure (ValInt 2)))
          @?= ([], Right (ValInt 1)),
      --
      testCase "catch keeps output of failed m1" $
        runEval
          ( catch
              (evalPrint "m1" >> failure "Oh no!")
              (evalPrint "m2" >> pure (ValInt 2))
          )
          @?= (["m1", "m2"], Right (ValInt 2)),
      --
      testCase "catch continues with continuation" $
        runEval
          ( do
              v <- catch (failure "Oh no!") (pure $ ValInt 1)
              evalPrint "after"
              pure v
          )
          @?= (["after"], Right (ValInt 1)),
      --
      testCase "Error after catch is not caught" $
        runEval
          ( do
              _ <- catch (pure $ ValInt 1) (pure $ ValInt 2)
              failure "later" :: EvalM Val
          )
          @?= ([], Left "later"),
      --
      testCase "Nested catch" $
        runEval (catch (catch (failure "a") (failure "b")) (pure $ ValInt 3))
          @?= ([], Right (ValInt 3)),
      --
      testCase "localEnv reaches m1 of catch" $
        runEval
          ( localEnv (envExtend "x" (ValInt 7)) $
              catch (eval (Var "x")) (pure $ ValInt 0)
          )
          @?= ([], Right (ValInt 7)),
      --
      testCase "localEnv reaches m2 of catch" $
        runEval
          ( localEnv (envExtend "x" (ValInt 7)) $
              catch (failure "Oh no!") (eval (Var "x"))
          )
          @?= ([], Right (ValInt 7)),
      --
      testCase "TryCatch inside Let" $
        eval' (Let "x" (CstInt 1) (TryCatch (Var "y") (Var "x")))
          @?= ([], Right (ValInt 1)),
      --
      testCase "KvPutOp then KvGetOp" $
        runEval (Free $ KvPutOp (ValInt 0) (ValInt 1) $ Free $ KvGetOp (ValInt 0) pure)
          @?= ([], Right (ValInt 1)),
      --
      testCase "evalKvPut then evalKvGet" $
        runEval (evalKvPut (ValInt 0) (ValInt 1) >> evalKvGet (ValInt 0))
          @?= ([], Right (ValInt 1)),
      --
      testCase "evalKvGet (missing key)" $
        runEval (evalKvGet (ValInt 0))
          @?= ([], Left "Invalid key: ValInt 0"),
      --
      testCase "evalKvPut replaces existing key" $
        runEval
          ( do
              evalKvPut (ValInt 0) (ValInt 1)
              evalKvPut (ValInt 0) (ValInt 2)
              evalKvGet (ValInt 0)
          )
          @?= ([], Right (ValInt 2)),
      --
      testCase "evalKvPut keeps other keys" $
        runEval
          ( do
              evalKvPut (ValInt 0) (ValInt 1)
              evalKvPut (ValInt 1) (ValInt 2)
              evalKvGet (ValInt 0)
          )
          @?= ([], Right (ValInt 1)),
      --
      testCase "Bool keys and values" $
        runEval
          ( do
              evalKvPut (ValBool True) (ValBool False)
              evalKvGet (ValBool True)
          )
          @?= ([], Right (ValBool False)),
      --
      testCase "ValInt and ValBool keys are distinct" $
        runEval
          ( do
              evalKvPut (ValInt 1) (ValInt 1)
              evalKvGet (ValBool True)
          )
          @?= ([], Left "Invalid key: ValBool True"),
      --
      testCase "KvPut inside localEnv is kept" $
        runEval
          ( do
              localEnv (const envEmpty) $ evalKvPut (ValInt 0) (ValInt 1)
              evalKvGet (ValInt 0)
          )
          @?= ([], Right (ValInt 1)),
      --
      testCase "eval KvPut returns the value" $
        eval' (KvPut (CstInt 0) (CstInt 1))
          @?= ([], Right (ValInt 1)),
      --
      testCase "eval KvPut then KvGet" $
        eval' (Let "_" (KvPut (CstInt 0) (CstInt 1)) (KvGet (CstInt 0)))
          @?= ([], Right (ValInt 1)),
      --
      testCase "eval KvGet (missing key)" $
        eval' (KvGet (CstInt 0))
          @?= ([], Left "Invalid key: ValInt 0"),
      --
      testCase "KvGet missing key caught by TryCatch" $
        eval' (TryCatch (KvGet (CstInt 0)) (CstInt 5))
          @?= ([], Right (ValInt 5))
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ()),
      --
      testCase "TryCatchOp (m1 fails)" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $
              Free $
                TryCatchOp (failure "Oh no!") (pure $ ValInt 1) pure
        (out, res) @?= ([], Right (ValInt 1)),
      --
      testCase "TryCatch (no error)" $ do
        (out, res) <- captureIO [] $ evalIO' (TryCatch (CstInt 5) divZero)
        (out, res) @?= ([], Right (ValInt 5)),
      --
      testCase "TryCatch (both fail)" $ do
        (out, res) <- captureIO [] $ evalIO' (TryCatch badEql divZero)
        (out, res) @?= ([], Left "Division by zero"),
      --
      testCase "catch prints of m1 and m2" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $
              catch
                (evalPrint "m1" >> failure "Oh no!")
                (evalPrint "m2" >> pure (ValInt 2))
        (out, res) @?= (["m1", "m2"], Right (ValInt 2)),
      --
      testCase "catch does not run m2 if m1 succeeds" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $
              catch
                (evalPrint "m1" >> pure (ValInt 1))
                (evalPrint "m2" >> pure (ValInt 2))
        (out, res) @?= (["m1"], Right (ValInt 1)),
      --
      testCase "TryCatch inside Let" $ do
        (out, res) <-
          captureIO [] $
            evalIO' (Let "x" (CstInt 1) (TryCatch (Var "y") (Var "x")))
        (out, res) @?= ([], Right (ValInt 1)),
      --
      testCase "KvPutOp then KvGetOp" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $
              Free $
                KvPutOp (ValInt 0) (ValInt 1) $
                  Free $
                    KvGetOp (ValInt 0) pure
        (out, res) @?= ([], Right (ValInt 1)),
      --
      testCase "evalKvPut replaces existing key" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalKvPut (ValInt 0) (ValInt 1)
              evalKvPut (ValInt 0) (ValInt 2)
              evalKvGet (ValInt 0)
        (out, res) @?= ([], Right (ValInt 2)),
      --
      testCase "evalKvPut keeps other keys" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalKvPut (ValInt 0) (ValInt 1)
              evalKvPut (ValInt 1) (ValBool True)
              a <- evalKvGet (ValInt 0)
              b <- evalKvGet (ValInt 1)
              pure (a, b)
        (out, res) @?= ([], Right (ValInt 1, ValBool True)),
      --
      testCase "Bool keys and values" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalKvPut (ValBool True) (ValBool False)
              evalKvGet (ValBool True)
        (out, res) @?= ([], Right (ValBool False)),
      --
      testCase "eval KvPut then KvGet" $ do
        (out, res) <-
          captureIO [] $
            evalIO' (Let "_" (KvPut (CstInt 0) (CstInt 1)) (KvGet (CstInt 0)))
        (out, res) @?= ([], Right (ValInt 1)),
      --
      testCase "KvPut inside successful catch is kept" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              ( Let
                  "_"
                  (TryCatch (KvPut (CstInt 0) (CstInt 1)) (CstInt 0))
                  (KvGet (CstInt 0))
              )
        (out, res) @?= ([], Right (ValInt 1)),
      --
      testCase "KvPut of failed m1 is visible in m2" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $
              catch
                (evalKvPut (ValInt 0) (ValInt 1) >> failure "Oh no!")
                (evalKvGet (ValInt 0))
        (out, res) @?= ([], Right (ValInt 1)),
      --
      testCase "Missing key test" $ do
        (_, res) <-
          captureIO ["ValInt 1"] $
            runEvalIO $
              Free $
                KvGetOp (ValInt 0) $
                  \val -> pure val
        res @?= Right (ValInt 1),
      --
      testCase "Missing key prompt" $ do
        (out, res) <-
          captureIO ["ValInt 1"] $
            runEvalIO $
              evalKvGet (ValInt 0)
        (out, res)
          @?= (["Invalid key: ValInt 0. Enter a replacement: "], Right (ValInt 1)),
      --
      testCase "Missing key (Bool replacement)" $ do
        (_, res) <-
          captureIO ["ValBool True"] $
            runEvalIO $
              evalKvGet (ValInt 0)
        res @?= Right (ValBool True),
      --
      testCase "Missing key (invalid input)" $ do
        (_, res) <-
          captureIO ["lol"] $
            runEvalIO $
              evalKvGet (ValInt 0)
        res @?= Left "Invalid value input: lol",
      --
      testCase "Missing key via eval" $ do
        (_, res) <-
          captureIO ["ValInt 5"] $
            evalIO' (KvGet (CstInt 0))
        res @?= Right (ValInt 5),
      --
      testCase "Existing key does not prompt" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalKvPut (ValInt 0) (ValInt 1)
              evalKvGet (ValInt 0)
        (out, res) @?= ([], Right (ValInt 1)),
      --
      testCase "Replacement is not stored in DB" $ do
        (_, res) <-
          captureIO ["ValInt 1", "ValInt 2"] $
            runEvalIO $ do
              a <- evalKvGet (ValInt 0)
              b <- evalKvGet (ValInt 0)
              pure (a, b)
        res @?= Right (ValInt 1, ValInt 2),
      --
      testCase "DB is cleared between runs" $ do
        _ <-
          captureIO [] $
            runEvalIO $
              evalKvPut (ValInt 0) (ValInt 1)
        (_, res) <-
          captureIO ["ValInt 9"] $
            runEvalIO $
              evalKvGet (ValInt 0)
        res @?= Right (ValInt 9)
        -- NOTE: This test will give a runtime error unless you replace the
        -- version of `eval` in `APL.Eval` with a complete version that supports
        -- `Print`-expressions. Uncomment at your own risk.
        -- testCase "print 2" $ do
        --    (out, res) <-
        --      captureIO [] $
        --        evalIO' $
        --          Print "This is also 1" $
        --            Print "This is 1" $
        --              CstInt 1
        --    (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1)
    ]
