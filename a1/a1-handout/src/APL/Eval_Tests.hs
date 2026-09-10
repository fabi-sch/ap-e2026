module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- The Y combinator in a form suitable for strict evaluation.
yComb :: Exp
yComb =
  Lambda "f" $
    Apply
      (Lambda "g" (Apply (Var "g") (Var "g")))
      ( Lambda
          "g"
          ( Apply
              (Var "f")
              (Lambda "a" (Apply (Apply (Var "g") (Var "g")) (Var "a")))
          )
      )

fact :: Exp
fact =
  Apply yComb $
    Lambda "rec" $
      Lambda "n" $
        If
          (Eql (Var "n") (CstInt 0))
          (CstInt 1)
          (Mul (Var "n") (Apply (Var "rec") (Sub (Var "n") (CstInt 1))))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval envEmpty (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",
      --
      testCase "Sub" $
        eval envEmpty (Sub (CstInt 2) (CstInt 5))
          @?= Right (ValInt (-3)),
      --
      testCase "Div" $
        eval envEmpty (Div (CstInt 7) (CstInt 3))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        eval envEmpty (Div (CstInt 7) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval envEmpty (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval envEmpty (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql (false)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql (true)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "If" $
        eval envEmpty (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= Right (ValInt 2),
      --
      testCase "Let" $
        eval envEmpty (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        eval
          envEmpty
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= Right (ValBool True),
      --
      testCase "ForLoop" $
        eval
          envEmpty
          ( ForLoop
              ("p", CstInt 0)
              ("i", CstInt 10)
              (Add (Var "p") (Var "i"))
          )
          @?= Right (ValInt 45),
      --
      testCase "ForLoop non-int bound" $
        eval
          envEmpty
          ( ForLoop
              ("p", CstInt 0)
              ("i", CstBool True)
              (Add (Var "p") (Var "i"))
          )
          @?= Left "Non-integral bound",
      --
      testCase "ForLoop non-int initial" $
        eval
          envEmpty
          ( ForLoop
              ("p", CstBool True)
              ("i", CstInt 10)
              (Add (Var "p") (Var "i"))
          )
          @?= Left "Non-integral initial",
      --
      testCase "ForLoop non-int body" $
        eval
          envEmpty
          ( ForLoop
              ("p", CstInt 0)
              ("i", CstInt 10)
              (CstBool True)
          )
          @?= Left "Non-integral body",
      --
      testCase "ForLoop increasing order" $
        eval
          envEmpty
          ( ForLoop
              ("p", CstInt 1)
              ("i", CstInt 3)
              (Sub (Mul (Var "p") (CstInt 2)) (Var "i"))
          )
          @?= Right (ValInt 4),
      --
      testCase "ForLoop includes i=0" $
        eval
          envEmpty
          (ForLoop ("p", CstInt 1) ("i", CstInt 3) (Mul (Var "p") (Var "i")))
          @?= Right (ValInt 0),
      --
      testCase "ForLoop bound 1 runs once" $
        eval
          envEmpty
          (ForLoop ("p", CstInt 5) ("i", CstInt 1) (Add (Var "p") (CstInt 100)))
          @?= Right (ValInt 105),
      --
      testCase "ForLoop bound 0 does nothing" $
        eval
          envEmpty
          (ForLoop ("p", CstInt 42) ("i", CstInt 0) (Add (Var "p") (Var "i")))
          @?= Right (ValInt 42),
      --
      testCase "ForLoop negative bound does nothing" $
        eval
          envEmpty
          (ForLoop ("p", CstInt 7) ("i", CstInt (-3)) (Add (Var "p") (Var "i")))
          @?= Right (ValInt 7),
      --
      testCase "Function Right" $
        eval
          envEmpty
          (Apply (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 3))
          @?= Right (ValInt 5),
      --
      testCase "Function Left" $
        eval envEmpty (Apply (Add (CstInt 3) (CstInt 4)) (CstInt 3))
          @?= Left "Not a function",
      --
      testCase "Function 1st param error" $
        eval envEmpty (Apply (Var "x") (CstInt 3))
          @?= Left "Unknown variable: x",
      --
      testCase "Function 2nd param error" $
        eval
          envEmpty
          (Apply (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) (Var "x"))
          @?= Left "Unknown variable: x",
      --
      testCase "Function (Shadowing)" $
        eval envEmpty (Let "x" (CstInt 10) (Apply (Lambda "x" (Var "x")) (CstInt 5)))
          @?= Right (ValInt 5),
      --
      testCase "TryCatch e1" $
        eval envEmpty (TryCatch (CstInt 0) (Var "x"))
          @?= Right (ValInt 0),
      --
      testCase "TryCatch e2" $
        eval envEmpty (TryCatch (Var "missing") (CstInt 1))
          @?= Right (ValInt 1),
      --
      testCase "TryCatch e2 fail" $
        eval envEmpty (TryCatch (Var "x") (Var "y"))
          @?= Left "Unknown variable: y",
      --
      testCase "Closures capture defining environment, not call-site" $
        eval
          envEmpty
          ( Let
              "x"
              (CstInt 1)
              ( Let
                  "f"
                  (Lambda "y" (Add (Var "x") (Var "y")))
                  (Let "x" (CstInt 100) (Apply (Var "f") (CstInt 2)))
              )
          )
          -- must use x=1 from closure creation, not x=100 from call site
          @?= Right (ValInt 3),
      --
      testCase "Apply curried two-argument function" $
        eval
          envEmpty
          ( Apply
              (Apply (Lambda "x" (Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 2))
              (CstInt 3)
          )
          @?= Right (ValInt 5),
      --
      testCase "Factorial via Y combinator" $
        eval envEmpty (Apply fact (CstInt 5))
          @?= Right (ValInt 120)
    ]
