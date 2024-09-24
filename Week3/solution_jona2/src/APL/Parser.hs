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
    manyTill,
    notFollowedBy,
    parse,
    satisfy,
    some,
    try
  )
import Text.Megaparsec.Char (space, char)

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
    "let",
    "in",
    "try",
    "catch",
    "print",
    "put",
    "get"
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

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

lBool :: Parser Bool
lBool =
  lexeme . try . choice $
    [ const True <$> lKeyword "true",
      const False <$> lKeyword "false"
    ]

-- String literals parser
lStringLiteral :: Parser String
lStringLiteral = lexeme $ char '"' >> manyTill lChar (char '"')
  where
    lChar = satisfy (/= '"')

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> lBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

pFExp :: Parser Exp
pFExp = do
  f <- pAtom
  args <- many pAtom
  pure $ foldl Apply f args

pLExp :: Parser Exp
pLExp =
  choice
    [ If
        <$> (lKeyword "if" *> pExp00)
        <*> (lKeyword "then" *> pExp00)
        <*> (lKeyword "else" *> pExp00),
      pFExp
    ]

-- Highest precedence **
pExp2 :: Parser Exp
pExp2 = do
  x <- pLExp
  choice
    [ do
        lString "**"
        y <- pExp2
        pure $ Pow x y,
      pure x
    ]

-- Next level: '*' and '/' operators (left-associative)
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

-- Next level: '+' and '-' operators (left-associative)
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

-- Lowest precedence: '==' operator (left-associative)
pExp00 :: Parser Exp
pExp00 = pExp0 >>= chain
  where
    chain x =
      choice
        [ do
            lString "=="
            y <- pExp0
            chain $ Eql x y,
          pure x
        ]

-- Parsing 'print', 'get', and 'put' constructs
pExp000 :: Parser Exp
pExp000 = choice
  [ pPrint,
    pGet,
    pPut,
    pExp00
  ]

pPrint :: Parser Exp
pPrint = do
  lKeyword "print"
  s <- lStringLiteral
  e <- pExp00
  pure $ Print s e

pGet :: Parser Exp
pGet = do
  lKeyword "get"
  e <- pExp00
  pure $ KvGet e

pPut :: Parser Exp
pPut = do
  lKeyword "put"
  e1 <- pExp00
  e2 <- pExp00
  pure $ KvPut e1 e2

pExp :: Parser Exp
pExp = pExp000

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
