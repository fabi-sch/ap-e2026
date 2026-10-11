module APL.Tests
  ( properties
  )
where

import APL.Parser (parseAPL)
import APL.AST (Exp (..), subExp, VName, printExp)
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp)
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , checkCoverage
  , oneof
  , sized
  , withMaxSuccess
  , suchThat
  , frequency
  , elements
  , chooseInt
  , vectorOf
  , (===)
  )

instance Arbitrary Exp where
  arbitrary = sized (genExp [])

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

genVar :: Gen VName
genVar = do
    n <- chooseInt (1, 3)
    alpha <- elements ['a' .. 'z']
    alphaNums <- vectorOf n $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
    pure (alpha : alphaNums)


genExp :: [VName] -> Int -> Gen Exp
genExp env 0 = oneof [CstInt . abs <$> arbitrary, CstBool <$> arbitrary]
genExp env size =
  frequency $
    [(200, Var <$> elements env) | not (null env)]++
    [ (3, CstInt . abs <$> arbitrary)
    , (3, CstBool <$> arbitrary)
    , (3, Add <$> genExp env halfSize <*> genExp env halfSize)
    , (2, Add <$> genExp env halfSize <*> (CstBool <$> arbitrary))
    , (3, Sub <$> genExp env halfSize <*> genExp env halfSize)
    , (2, Sub <$> genExp env halfSize <*> (CstBool <$> arbitrary))
    , (3, Mul <$> genExp env halfSize <*> genExp env halfSize)
    , (1, Mul <$> genExp env halfSize <*> (CstBool <$> arbitrary))
    , (3, Div <$> genExp env halfSize <*> genExp env halfSize)
    , (4, Div <$> genExp env halfSize <*> pure (CstInt 0))
    , (1, Div <$> genExp env halfSize <*> (CstBool <$> arbitrary))
    , (3, Pow <$> genExp env halfSize <*> genExp env halfSize)
    , (4, Pow <$> genExp env halfSize <*> pure (Sub (CstInt 0) (CstInt 1)))
    , (3, Pow <$> genExp env halfSize <*> (CstBool <$> arbitrary))
    , (3, Eql <$> genExp env halfSize <*> genExp env halfSize)
    , (3, Eql <$> genExp env halfSize <*> (CstBool <$> arbitrary))
    , (3, If <$> genExp env thirdSize <*> genExp env thirdSize <*> genExp env thirdSize)
    , (2, If <$> (CstInt . abs <$> arbitrary) <*> genExp env thirdSize <*> genExp env thirdSize)
    , (1, Var <$> genVar `suchThat` (`notElem` env))
    , (5, do
            v <- genVar
            let newEnv = v : env
            body <- genExp newEnv (size - 1)
            pure (Lambda v body))
    , (5, do
            v <- genVar
            let newEnv = v : env
            e1 <- genExp env halfSize
            e2 <- genExp newEnv halfSize
            pure (Let v e1 e2))
    , (3, Apply <$> genExp env halfSize <*> genExp env halfSize )
    , (3, TryCatch <$> genExp env halfSize <*> genExp env halfSize)
    ]
  where
    halfSize = size `div` 2
    thirdSize = size `div` 3

expCoverage :: Exp -> Property
expCoverage e = checkCoverage
  . cover 20 (any isDomainError (checkExp e)) "domain error"
  . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
  . cover 20 (any isTypeError (checkExp e)) "type error"
  . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
  . cover 5 (any isVariableError (checkExp e)) "variable error"
  . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
  . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
  $ ()

parsePrinted :: Exp -> Property
parsePrinted s = parseAPL "" (printExp s) === Right s

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors _ = undefined

-- The number of tests is part of the specification of this test suite: some of
-- these properties fail only rarely.  Do not reduce it.
properties :: [(String, Property)]
properties =
  [ ("expCoverage", property $ withMaxSuccess 10000 expCoverage)
  , ("parsePrinted", property $ withMaxSuccess 10000 parsePrinted)
  , ("onlyCheckedErrors", property $ withMaxSuccess 10000 onlyCheckedErrors)
  ]
