module APL.Check_Tests (tests) where

import APL.AST (Exp (..))
import APL.Check (checkExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- Assert that the provided expression should pass the type checker.
testPos :: Exp -> TestTree
testPos e =
  testCase (show e) $
    checkExp e @?= Nothing

-- Assert that the provided expression should fail the type checker.
testNeg :: Exp -> TestTree
testNeg e =
  testCase (show e) $
    case checkExp e of
      Nothing -> assertFailure "expected error"
      Just _ -> pure ()

tests :: TestTree
tests =
  testGroup
    "Checking"
    [ testPos (CstInt 2),
      testNeg (Var "x"),
      testPos (Lambda "x" (Var "x")),
      testPos (Let "x" (CstInt 2) (Var "x")),
      testNeg (Let "x" (Var "x") (CstInt 1)),
      testPos
        ( ForLoop
            ("p", CstInt 0)
            ("i", CstInt 10)
            (Add (Var "p") (Var "i"))
        ),
      testNeg
        ( ForLoop
            ("p", CstInt 0)
            ("i", CstInt 10)
            (Var "j")
        ),
      testPos (Apply (Lambda "x" (Var "x")) (CstInt 4)),
      testNeg (Add (CstInt 1) (Var "y"))
    ]
