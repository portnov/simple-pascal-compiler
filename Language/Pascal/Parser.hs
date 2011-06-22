
module Language.Pascal.Parser where

import Data.Char
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language

import Language.Pascal.Types

type Parser a = Parsec String () a

pascal = P.makeTokenParser $ javaStyle {
           P.commentStart = "(*",
           P.commentEnd = "*)",
           P.reservedNames = ["program", "function", "begin", "end", "var",
                             "return", "if", "then", "else", "for", "to", "do"] }

symbol = P.symbol pascal
identifier = P.identifier pascal
semi = P.semi pascal
colon = P.colon pascal
comma = P.comma pascal
parens = P.parens pascal

annotate p = do
  pos <- getPosition
  x <- p
  return $ Ann {
    content = x,
    srcLine = sourceLine pos,
    srcColumn = sourceColumn pos }

pProgram :: Parser Program
pProgram = do
  symbol "program"
  identifier
  semi
  vars <- option [] pVars
  fns <- many pFunction
  symbol "begin"
  sts <- pStatement `sepEndBy1` semi 
  symbol "end."
  return $ Program vars fns sts

readType str =
  case map toLower str of
    "integer" -> TInteger
    "string" -> TString
    "boolean" -> TBool
    s         -> error $ "Unknown type: " ++ s

pVars :: Parser [Ann NameType]
pVars = do
  symbol "var"
  pNameType `sepEndBy1` semi 

pNameType :: Parser (Ann NameType)
pNameType = annotate $ do
  name <- identifier
  colon
  tp <- identifier
  return $ name ::: readType tp

pFunction :: Parser (Ann Function)
pFunction = annotate $ do
  symbol "function"
  name <- identifier
  args <- parens $ pNameType `sepBy` comma
  semi
  vars <- option [] pVars
  symbol "begin"
  body <- pStatement `sepEndBy1` semi
  symbol "end;"
  return $ Function name args vars body

pStatement :: Parser (Ann Statement)
pStatement =
      try pAssign
  <|> try pProcedureCall
  <|> try pReturn
  <|> try pIfThenElse
  <|> pFor

pAssign :: Parser (Ann Statement)
pAssign = annotate $ do
  var <- identifier
  symbol ":="
  expr <- pExpression
  return $ Assign var expr

pProcedureCall = annotate $ do
  name <- identifier
  args <- parens $ pExpression `sepBy` comma
  return $ Procedure name args

pReturn :: Parser (Ann Statement)
pReturn = annotate $ do
  symbol "return"
  x <- pExpression
  return $ Return x

pIfThenElse :: Parser (Ann Statement)
pIfThenElse = annotate $ do
  symbol "if"
  cond <- pExpression
  symbol "then"
  ok <- pBlock
  el <- option [] $ do
          symbol "else"
          pBlock
  return $ IfThenElse cond ok el

pBlock = try (one `fmap` pStatement) <|> do
           symbol "begin"
           sts <- pStatement `sepEndBy1` semi
           symbol "end;"
           return sts
  where
    one x = [x]

pFor = annotate $ do
  symbol "for"
  var <- identifier
  symbol ":="
  start <- pExpression
  symbol "to"
  end <- pExpression
  sts <- pBlock
  return $ For var start end sts

pExpression :: Parser (Ann Expression)
pExpression = 
      try (annotate $ Literal `fmap` pLiteral)
  <|> try pVariable
  <|> try (parens pExpression)
  <|> try pBinaryOp
  <|> pCall

pLiteral = try stringLit <|> try intLit <|> try boolLit
  where
    stringLit = LString `fmap` P.stringLiteral pascal
    intLit = LInteger `fmap` P.integer pascal 
    boolLit = try (symbol "true" >> return (LBool True)) <|> (symbol "false" >> return (LBool False))

pVariable = annotate $  Variable `fmap` identifier

pBinaryOp :: Parser (Ann Expression)
pBinaryOp = annotate $ do
    x <- pExpression
    op <- operation
    y <- pExpression
    return $ Op op x y
  where
    operation = try (symbol "+" >> return Add)
            <|> try (symbol "-" >> return Sub)
            <|> try (symbol "*" >> return Mul)
            <|> try (symbol "/" >> return Div)
            <|> try (symbol "%" >> return Mod)
            <|> try (symbol "^" >> return Pow)

pCall :: Parser (Ann Expression)
pCall = annotate $ do
  name <- identifier
  args <- parens $ pExpression `sepBy` comma
  return $ Call name args

