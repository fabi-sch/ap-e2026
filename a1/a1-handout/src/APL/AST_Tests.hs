module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [ testCase "Add" $
        printExp (Add (CstInt 2) (CstInt 5))
              @?= "(2 + 5)"
    ]
