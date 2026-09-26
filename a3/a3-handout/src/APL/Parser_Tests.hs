module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "123 " $ CstInt 123,
          parserTestFail "123f",
          parserTest "true" $ CstBool True,
          parserTest "false" $ CstBool False
        ],
      testGroup
        "Basic operators"
        [ parserTest "x+y" $ Add (Var "x") (Var "y"),
          parserTest "x-y" $ Sub (Var "x") (Var "y"),
          parserTest "x*y" $ Mul (Var "x") (Var "y"),
          parserTest "x/y" $ Div (Var "x") (Var "y")
        ],
      testGroup
        "Operator priority"
        [ parserTest "x+y+z" $ Add (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y-z" $ Sub (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z" $ Add (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x*y*z" $ Mul (Mul (Var "x") (Var "y")) (Var "z"),
          parserTest "x/y/z" $ Div (Div (Var "x") (Var "y")) (Var "z")
        ],
      testGroup
        "Function application"
        [ parserTest "x y" $ Apply (Var "x") (Var "y"),
          parserTest "x y z" $ Apply (Apply (Var "x") (Var "y")) (Var "z"),
          parserTest "x(y z)" $ Apply (Var "x") (Apply (Var "y") (Var "z")),
          parserTest "x y + z" $ Add (Apply (Var "x") (Var "y")) (Var "z"), 
          parserTestFail "x if x then y else z"
        ],
      testGroup
        "Equality and power operations"
        [ parserTest "x*y**z" $ Mul (Var "x") (Pow (Var "y") (Var "z")),
          parserTest "x+y==y+x" $ Eql (Add (Var "x") (Var "y")) (Add (Var "y") (Var "x")),
          parserTest "x*y**z+t" $ Add (Mul (Var "x") (Pow (Var "y") (Var "z"))) (Var "t"),
          parserTest "x*y**(z+t)" $ Mul (Var "x") (Pow (Var "y") (Add (Var "z") (Var "t"))),
          parserTest "x**y**z" $ Pow (Var "x") (Pow (Var "y") (Var "z")),
          parserTest "x==y==z" $ Eql (Eql (Var "x") (Var "y")) (Var "z"),
          parserTest "x**y*z" $ Mul (Pow (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z==w" $ Eql (Add (Var "x") (Mul (Var "y") (Var "z"))) (Var "w")
        ],
      testGroup
        "Print, put and get operations"
        [ parserTest "put x y" $ KvPut (Var "x") (Var "y"),
          parserTest "get x + y" $ Add (KvGet (Var "x")) (Var "y"),
          parserTest "getx" $ Var "getx",
          parserTest "print \"foo\" x" $ Print "foo" (Var "x"),
          parserTest "print \"hello world\" x" $ Print "hello world" (Var "x"),
          parserTest "print \"\" x" $ Print "" (Var "x"),
          parserTest "get (x + y)" $ KvGet (Add (Var "x") (Var "y")),
          parserTest "put x (y + z)" $ KvPut (Var "x") (Add (Var "y") (Var "z")),
          parserTestFail "get",
          parserTestFail "put x",
          parserTestFail "print x"
        ],
      testGroup
        "Lambdas, let-binding, loops and try-catch"
        [ parserTest "let x = y in z" $ Let "x" (Var "y") (Var "z"),
          parserTest "let x =y in z" $ Let "x" (Var "y") (Var "z"),
          parserTest "let x = y in z + 1" $ Let "x" (Var "y") (Add (Var "z") (CstInt 1)),
          parserTest "let x = 1 in let y = 2 in x + y" $
            Let "x" (CstInt 1) $
              Let "y" (CstInt 2) (Add (Var "x") (Var "y")),
          parserTest "(let x = y in z) + 1" $ Add (Let "x" (Var "y") (Var "z")) (CstInt 1),
          parserTest "letx" $ Var "letx",
          parserTestFail "let true = y in z",
          parserTestFail "x let v = 2 in v",
          parserTestFail "let",
          parserTestFail "let x = y",
          parserTest "\\x -> x + x" $ Lambda "x" (Add (Var "x") (Var "x")),
          parserTest "(\\x -> x) + x" $ Add (Lambda "x" (Var "x")) (Var "x"),
          parserTest "(\\x -> x) y" $ Apply (Lambda "x" (Var "x")) (Var "y"),
          parserTest "\\x -> \\y -> x y" $ Lambda "x" (Lambda "y" (Apply (Var "x") (Var "y"))),
          parserTestFail "\\true -> x",
          parserTestFail "\\x x",
          parserTest "try x catch y" $ TryCatch (Var "x") (Var "y"),
          parserTest "try x + 1 catch y + 1" $
            TryCatch (Add (Var "x") (CstInt 1)) (Add (Var "y") (CstInt 1)),
          parserTest "try try x catch y catch z" $
            TryCatch (TryCatch (Var "x") (Var "y")) (Var "z"),
          parserTest "tryx" $ Var "tryx",
          parserTestFail "try x",
          parserTest "loop x = 1 for i < n do x * 2" $
            ForLoop ("x", CstInt 1) ("i", Var "n") (Mul (Var "x") (CstInt 2)),
          parserTest "loop x = y + 1 for i < 10 do x + i" $
            ForLoop ("x", Add (Var "y") (CstInt 1)) ("i", CstInt 10) (Add (Var "x") (Var "i")),
          parserTest "loopy" $ Var "loopy",
          parserTestFail "loop for = 1 for i < n do x",
          parserTestFail "loop x = 1 for i < n"
        ],
      testGroup
        "Conditional expressions"
        [ parserTest "if x then y else z" $ If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then y else if x then y else z" $
            If (Var "x") (Var "y") $
              If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then (if x then y else z) else z" $
            If (Var "x") (If (Var "x") (Var "y") (Var "z")) (Var "z"),
          parserTest "1 + if x then y else z" $
            Add (CstInt 1) (If (Var "x") (Var "y") (Var "z"))
        ],
      testGroup
        "Lexing edge cases"
        [ parserTest "2 " $ CstInt 2,
          parserTest " 2" $ CstInt 2
        ]
    ]
