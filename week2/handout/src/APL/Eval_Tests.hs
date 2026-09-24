module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, Val (..), eval, runEval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "CstInt" $
        runEval (eval [] (CstInt 2)) @?= Right (ValInt 2),
      testCase "CstBool" $
        runEval (eval [] (CstBool True)) @?= Right (ValBool True),
      testCase "Add" $
        runEval (eval [] (Add (CstInt 2) (CstInt 3))) @?= Right (ValInt 5),
      testCase "AddNonInteger" $
        runEval (eval [] (Add (CstBool True) (CstInt 3))) @?= Left "Non-integer operators",
      testCase "Sub" $
        runEval (eval [] (Sub (CstInt 2) (CstInt 3))) @?= Right (ValInt (-1)),
      testCase "Mul" $
        runEval (eval [] (Mul (CstInt 2) (CstInt 3))) @?= Right (ValInt 6),
      testCase "Div" $
        runEval (eval [] (Div (CstInt 6) (CstInt 3))) @?= Right (ValInt 2),
      testCase "DivZero" $
        runEval (eval [] (Div (CstInt 6) (CstInt 0))) @?= Left "Zero-division",
      testCase "Pow" $
        runEval (eval [] (Pow (CstInt 2) (CstInt 3))) @?= Right (ValInt 8),
      testCase "PowNegative" $
        runEval (eval [] (Pow (CstInt 2) (CstInt (-1)))) @?= Left "Negative-pow",
      testCase "Nested" $
        runEval (eval [] (Add (CstInt 2) (Mul (CstInt 3) (CstInt 4)))) @?= Right (ValInt 14),
      testCase "ErrorPropagation" $
        runEval (eval [] (Add (Div (CstInt 1) (CstInt 0)) (CstInt 5))) @?= Left "Zero-division",
      testCase "EqlIntTrue" $
        runEval (eval [] (Eql (CstInt 6) (CstInt 6))) @?= Right (ValBool True),
      testCase "EqlIntFalse" $
        runEval (eval [] (Eql (CstInt 4) (CstInt 6))) @?= Right (ValBool False),
      testCase "EqlBoolTrue" $
        runEval (eval [] (Eql (CstBool False) (CstBool False))) @?= Right (ValBool True),
      testCase "EqlBoolFalse" $
        runEval (eval [] (Eql (CstBool True) (CstBool False))) @?= Right (ValBool False),
      testCase "EqlDifferentTypes" $
        runEval (eval [] (Eql (CstInt 4) (CstBool True))) @?= Left "Invalid operands to equality",
      testCase "VarUnknown" $
        runEval (eval [] (Var "x")) @?= Left "Unknown variable: x",
      testCase "IfTrue" $
        runEval (eval [] (If (CstBool True) (CstInt 6) (CstInt 4))) @?= Right (ValInt 6),
      testCase "IfFalse" $
        runEval (eval [] (If (CstBool False) (CstInt 6) (CstInt 4))) @?= Right (ValInt 4),
      testCase "IfNonBoolean" $
        runEval (eval [] (If (CstInt 4) (CstInt 5) (CstInt 6))) @?= Left "Non-boolean conditional",
      testCase "IfShortCircuitTrue" $
        runEval (eval [] (If (CstBool True) (CstInt 5) (Div (CstInt 1) (CstInt 0)))) @?= Right (ValInt 5),
      testCase "IfShortCircuitFalse" $
        runEval (eval [] (If (CstBool False) (Div (CstInt 1) (CstInt 0)) (CstInt 5))) @?= Right (ValInt 5),
      testCase "LetInt" $
        runEval (eval [] (Let "x" (CstInt 3) (Add (Var "x") (Var "x")))) @?= Right (ValInt 6),
      testCase "LetBool" $
        runEval (eval [] (Let "x" (CstBool True) (Eql (Var "x") (Var "x")))) @?= Right (ValBool True),
      testCase "LetShadowing" $
        runEval (eval [] (Let "x" (CstInt 1) (Let "x" (CstInt 2) (Var "x")))) @?= Right (ValInt 2),
      testCase "ForLoopSum" $
        runEval
          ( eval
              []
              ( ForLoop
                  ("acc", CstInt 0)
                  ("i", CstInt 5)
                  (Add (Var "acc") (Var "i"))
              )
          )
          @?= Right (ValInt 10),
      testCase "ForLoopZeroIterations" $
        runEval
          ( eval
              []
              ( ForLoop
                  ("acc", CstInt 42)
                  ("i", CstInt 0)
                  (Add (Var "acc") (Var "i"))
              )
          )
          @?= Right (ValInt 42),
      testCase "ForLoopInvalidBound" $
        runEval
          ( eval
              []
              ( ForLoop
                  ("acc", CstInt 0)
                  ("i", CstBool True)
                  (Var "acc")
              )
          )
          @?= Left "Invalid bound",
      testCase "Lambda" $
        runEval (eval [] (Lambda "x" (Var "x")))
          @?= Right (ValFun [] "x" (Var "x")),
      testCase "ApplyIdentity" $
        runEval (eval [] (Apply (Lambda "x" (Var "x")) (CstInt 5)))
          @?= Right (ValInt 5),
      testCase "ApplyAddOne" $
        runEval (eval [] (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstInt 4)))
          @?= Right (ValInt 5),
      testCase "ApplyNotAFunction" $
        runEval (eval [] (Apply (CstInt 1) (CstInt 2)))
          @?= Left "Not a function",
      testCase "ApplyArgError" $
        runEval (eval [] (Apply (Lambda "x" (Var "x")) (Div (CstInt 1) (CstInt 0))))
          @?= Left "Zero-division",
      testCase "ApplyClosureCapturesEnv" $
        runEval
          ( eval
              []
              ( Let
                  "x"
                  (CstInt 10)
                  (Apply (Lambda "y" (Add (Var "x") (Var "y"))) (CstInt 5))
              )
          )
          @?= Right (ValInt 15),
      testCase "ApplyClosureIgnoresLaterRebinding" $
        runEval
          ( eval
              []
              ( Let
                  "x"
                  (CstInt 1)
                  ( Let
                      "f"
                      (Lambda "y" (Var "x"))
                      (Let "x" (CstInt 2) (Apply (Var "f") (CstInt 0)))
                  )
              )
          )
          @?= Right (ValInt 1),
      testCase "ApplyCurried" $
        runEval
          ( eval
              []
              ( Apply
                  (Apply (Lambda "x" (Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 3))
                  (CstInt 4)
              )
          )
          @?= Right (ValInt 7)
    ]
