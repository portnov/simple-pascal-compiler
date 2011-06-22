{-# LANGUAGE RecordWildCards, TypeOperators, StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
module Language.Pascal.Parser where

import Data.Char
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec.Expr

import Language.Pascal.Types

type Parser a = Parsec String () a

pascal = P.makeTokenParser $ javaStyle {
           P.commentStart = "(*",
           P.commentEnd = "*)",
           P.reservedNames = ["program", "function", "begin", "end", "var", "true", "false",
                             ":=", "return", "if", "then", "else", "for", "to", "do"] }

symbol = P.symbol pascal
reserved = P.reserved pascal
reservedOp = P.reservedOp pascal
identifier = P.identifier pascal
semi = P.semi pascal
colon = P.colon pascal
comma = P.comma pascal
dot = P.dot pascal
parens = P.parens pascal

annotate p = do
  pos <- getPosition
  x <- p
  return $ SrcPos {
    content = x,
    srcLine = sourceLine pos,
    srcColumn = sourceColumn pos }

pProgram :: Parser (Program :~ SrcPos)
pProgram = annotate $ do
  reserved "program"
  identifier
  semi
  vars <- option [] pVars
  fns <- many pFunction
  reserved "begin"
  sts <- pStatement `sepEndBy1` semi 
  reserved "end"
  dot
  return $ Program vars fns sts

readType str =
  case map toLower str of
    "integer" -> TInteger
    "string"  -> TString
    "boolean" -> TBool
    "void"    -> TVoid
    s         -> error $ "Unknown type: " ++ s

pVars :: Parser [SrcPos NameType]
pVars = do
  reserved "var"
  pNameType `sepEndBy1` semi 

pNameType :: Parser (SrcPos NameType)
pNameType = annotate $ do
  name <- identifier
  colon
  tp <- identifier
  return $ name ::: readType tp

pFunction :: Parser (Function :~ SrcPos)
pFunction = annotate $ do
  reserved "function"
  name <- identifier
  args <- parens $ pNameType `sepBy` comma
  colon
  res <- identifier
  semi
  vars <- option [] pVars
  reserved "begin"
  body <- pStatement `sepEndBy1` semi
  reserved "end"
  semi
  return $ Function name args (readType res) vars body

pStatement :: Parser (Statement :~ SrcPos)
pStatement =
      try pAssign
  <|> try pProcedureCall
  <|> try pReturn
  <|> try pIfThenElse
  <|> pFor

pAssign :: Parser (Statement :~ SrcPos)
pAssign = annotate $ do
  var <- identifier
  symbol ":="
  expr <- pExpression
  return $ Assign var expr

pProcedureCall = annotate $ do
  name <- identifier
  args <- parens $ pExpression `sepBy` comma
  return $ Procedure name args

pReturn :: Parser (Statement :~ SrcPos)
pReturn = annotate $ do
  reserved "return"
  x <- pExpression
  return $ Return x

pIfThenElse :: Parser (Statement :~ SrcPos)
pIfThenElse = annotate $ do
  reserved "if"
  cond <- pExpression
  reserved "then"
  ok <- pBlock
  el <- option [] $ do
          reserved "else"
          pBlock
  return $ IfThenElse cond ok el

pBlock = try (one `fmap` pStatement) <|> do
           reserved "begin"
           sts <- pStatement `sepEndBy1` semi
           reserved "end"
           semi
           return sts
  where
    one x = [x]

pFor = annotate $ do
  reserved "for"
  var <- identifier
  reserved ":="
  start <- pExpression
  reserved "to"
  end <- pExpression
  reserved "do"
  sts <- pBlock
  return $ For var start end sts

pExpression :: Parser (Expression :~ SrcPos)
pExpression = buildExpressionParser table term <?> "expression"
  where
    table = [
            [binary "^" (Pow) AssocLeft],
            [binary "*" (Mul) AssocLeft, binary "/" (Div) AssocLeft, binary "%" (Mod) AssocLeft ]
          , [binary "+" (Add) AssocLeft, binary "-" (Sub)   AssocLeft ]
          ]
    binary  name fun assoc = Infix (op name fun) assoc
    op name fun = do
      pos <- getPosition
      reservedOp name
      return $ \x y -> SrcPos {
        content = Op fun x y,
        srcLine = sourceLine pos,
        srcColumn = sourceColumn pos }


term = parens pExpression
   <|> (annotate $ Literal `fmap` pLiteral)
   <|> pVariable

        
pLiteral = try stringLit <|> try intLit <|> boolLit
  where
    stringLit = LString `fmap` P.stringLiteral pascal
    intLit = LInteger `fmap` P.integer pascal 
    boolLit = try (reserved "true" >> return (LBool True)) <|> (reserved "false" >> return (LBool False))

pVariable = annotate $  Variable `fmap` identifier

pBinaryOp :: Parser (Expression :~ SrcPos)
pBinaryOp = annotate $ do
    x <- pExpression
    op <- operation
    y <- pExpression
    return $ Op op x y
  where
    operation = try (reservedOp "+" >> return Add)
            <|> try (reservedOp "-" >> return Sub)
            <|> try (reservedOp "*" >> return Mul)
            <|> try (reservedOp "/" >> return Div)
            <|> try (reservedOp "%" >> return Mod)
            <|> (reservedOp "^" >> return Pow)

pCall :: Parser (Expression :~ SrcPos)
pCall = annotate $ do
  name <- identifier
  args <- parens $ pExpression `sepBy` comma
  return $ Call name args

parseSource :: FilePath -> IO (Program :~ SrcPos)
parseSource path = do
  src <- readFile path
  case parse pProgram path src of
    Left err -> fail $ show err
    Right x -> return x
