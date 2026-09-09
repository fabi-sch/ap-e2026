module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), eval, envEmpty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [
      testCase "CstInt" $
        eval envEmpty (CstInt 2) @?= Right (ValInt 2),
      testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 3)) @?= Right (ValInt 5),
      testCase "Sub" $
        eval envEmpty (Sub (CstInt 2) (CstInt 3)) @?= Right (ValInt (-1)),
      testCase "Mul" $
        eval envEmpty (Mul (CstInt 2) (CstInt 3)) @?= Right (ValInt 6),
      testCase "Div" $
        eval envEmpty (Div (CstInt 6) (CstInt 3)) @?= Right (ValInt 2),
      testCase "DivZero" $
        eval envEmpty (Div (CstInt 6) (CstInt 0)) @?= Left "Division by zero",
      testCase "Pow" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3)) @?= Right (ValInt 8),
      testCase "PowNegative" $
        eval envEmpty (Pow (CstInt 2) (CstInt $ -1)) @?= Left "Negative exponent",
      testCase "Nested" $
        eval envEmpty (Add (CstInt 2) (Mul (CstInt 3) (CstInt 4))) @?= Right (ValInt 14),
      testCase "ErrorPropagation" $
        eval envEmpty (Add (Div (CstInt 1) (CstInt 0)) (CstInt 5)) @?= Left "Division by zero",
      testCase "CstBool" $
        eval envEmpty (CstBool True) @?= Right (ValBool True),
      testCase "EqlIntTrue" $
        eval envEmpty (Eql (CstInt 6) (CstInt 6)) @?= Right (ValBool True),
      testCase "EqlIntFalse" $
        eval envEmpty (Eql (CstInt 4) (CstInt 6)) @?= Right (ValBool False),
      testCase "EqlBoolTrue" $
        eval envEmpty (Eql (CstBool False) (CstBool False)) @?= Right (ValBool True),
      testCase "EqlBoolFalse" $
        eval envEmpty (Eql (CstBool True) (CstBool False)) @?= Right (ValBool False),
      testCase "EqlErrorDiffTypes" $
        eval envEmpty (Eql (CstInt 4) (CstBool True)) @?= Left "Cannot compare values of different types",
      testCase "IfTrue" $
        eval envEmpty (If (CstBool True) (CstInt 6) (CstInt 4)) @?= Right (ValInt 6),
      testCase "IfFalse" $
        eval envEmpty (If (CstBool False) (CstInt 6) (CstInt 4)) @?= Right (ValInt 4),
      testCase "IfErrorInt" $
        eval envEmpty (If (CstInt 4) (CstInt 5) (CstInt 6)) @?= Left "Non-boolean conditional",
      testCase "IfShortCircuitTrue" $
        eval envEmpty (If (CstBool True) (CstInt 5) (Div (CstInt 1) (CstInt 0))) @?= Right (ValInt 5),
      testCase "IfShortCircuitFalse" $
        eval envEmpty (If (CstBool False) (Div (CstInt 1) (CstInt 0)) (CstInt 5)) @?= Right (ValInt 5),
      testCase "LetInt" $
        eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "x"))) @?= Right (ValInt 6),
      testCase "LetBool" $
        eval envEmpty (Let "x" (CstBool True) (Eql (Var "x") (Var "x"))) @?= Right (ValBool True)
    ]
