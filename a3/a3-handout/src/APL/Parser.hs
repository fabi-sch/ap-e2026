module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "print",
    "get",
    "put",
    "in",
    "loop",
    "for",
    "do",
    "try",
    "catch",
    "let"
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lStringLit :: Parser String
lStringLit = lexeme $ chunk "\"" *> many (satisfy (/= '"')) <* chunk "\""

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

pBool :: Parser Bool
pBool =
  choice $
    [ const True <$> lKeyword "true",
      const False <$> lKeyword "false"
    ]

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> pBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

pLExp :: Parser Exp
pLExp =
  choice
    [ If
        <$> (lKeyword "if" *> pExp)
        <*> (lKeyword "then" *> pExp)
        <*> (lKeyword "else" *> pExp),

      Print
        <$> (lKeyword "print" *> lStringLit)
        <*> pAtom,

      KvGet
        <$> (lKeyword "get" *> pAtom),

      KvPut
        <$> (lKeyword "put" *> pAtom)
        <*> pAtom,

      Let
        <$> (lKeyword "let" *> lVName)
        <*> (lString "=" *> pExp)
        <*> (lKeyword "in" *> pExp),

      do
          v1 <- (lKeyword "loop" *> lVName)
          e1 <- (lString "=" *> pExp)
          v2 <- (lKeyword "for" *> lVName)
          e2 <- (lString "<" *> pExp)
          e3 <- (lKeyword "do" *> pExp)
          pure $ ForLoop (v1, e1) (v2, e2) e3,

      TryCatch
        <$> (lKeyword "try" *> pExp)
        <*> (lKeyword "catch" *> pExp),

      Lambda
        <$> (lString "\\" *> lVName)
        <*> (lString "->" *> pExp),
      pFExp
    ]

pFExp :: Parser Exp
pFExp = pAtom >>= chain
  where
    chain x =
      choice
        [ do
            y <- pAtom
            chain $ Apply x y,
          pure x
        ]

pExp2 :: Parser Exp
pExp2 = pLExp >>= chain
  where
    chain x =
      choice
        [ do
            try (lString "**")
            y <- pExp2
            chain $ Pow x y,
          pure x
        ]

pExp1 :: Parser Exp
pExp1 = pExp2 >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pExp2
            chain $ Mul x y,
          do
            lString "/"
            y <- pExp2
            chain $ Div x y,
          pure x
        ]

pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp1
            chain $ Add x y,
          do
            lString "-"
            y <- pExp1
            chain $ Sub x y,
          pure x
        ]

pExp_1 :: Parser Exp
pExp_1 = pExp0 >>= chain
  where
    chain x = 
      choice
      [ do
          lString "=="
          y <- pExp0
          chain $ Eql x y,
        pure x        
      ]

pExp :: Parser Exp
pExp = pExp_1

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
