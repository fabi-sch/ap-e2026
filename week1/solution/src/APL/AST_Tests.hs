module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "EValuation"
    [ 
        testCase "CstInt" $
          printExp (CstInt 5)
            @?= "5",
        --
        testCase "CstBool true" $
          printExp (CstBool True)
            @?= "true",
        --
        testCase "CstBool false" $
          printExp (CstBool False)
            @?= "false",
        --
        testCase "Var" $
          printExp (Var "x")
            @?= "x",
        --
        testCase "Sub" $
          printExp (Sub (CstInt 5) (CstInt 2))
            @?= "(5 - 2)",
        --
        testCase "Mul" $
          printExp (Mul (CstInt 3) (CstInt 4))
            @?= "(3 * 4)",
        --
        testCase "Div" $
          printExp (Div (CstInt 10) (CstInt 2))
            @?= "(10 / 2)",
        --
        testCase "Pow" $
          printExp (Pow (CstInt 2) (CstInt 3))
            @?= "(2 ** 3)",
        --
        testCase "Eql" $
          printExp (Eql (CstInt 1) (CstInt 1))
            @?= "(1 == 1)",
        --
        testCase "If" $
          printExp (If (CstBool True) (CstInt 1) (CstInt 2))
            @?= "(if true then 1 else 2)",
        --
        testCase "Let" $
          printExp (Let "x" (CstInt 1) (Var "x"))
            @?= "(let x = 1 in x)",
        --
        testCase "ForLoop" $
          printExp (ForLoop ("p", CstInt 0) ("i", CstInt 10) (Add (Var "p") (Var "i")))
            @?= "(loop p = 0 for i < 10 do (p + i))",
        --
        testCase "Nested expressions" $
          printExp (Add (Mul (CstInt 2) (CstInt 3)) (CstInt 4))
            @?= "((2 * 3) + 4)"
    ]
