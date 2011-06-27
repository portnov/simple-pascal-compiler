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
                             ":=", "return", "if", "then", "else", "for", "to", "do",
                             "exit", "procedure"] }

symbol = P.symbol pascal
reserved = P.reserved pascal
reservedOp = P.reservedOp pascal
identifier = P.identifier pascal
semi = P.semi pascal
colon = P.colon pascal
comma = P.comma pascal
dot = P.dot pascal
parens = P.parens pascal

withAnnotation :: Parser x -> Parser (Annotate x SrcPos)
withAnnotation p = do
  pos <- getPosition
  x <- p
  return $ Annotate x $ SrcPos {
    srcLine = sourceLine pos,
    srcColumn = sourceColumn pos }

pProgram :: Parser (Program :~ SrcPos)
pProgram = withAnnotation $ do
  reserved "program"
  identifier
  semi
  vars <- option [] pVars
  fns <- many (try pFunction <|> pProcedure)
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

pVars :: Parser [Annotate NameType SrcPos]
pVars = do
  reserved "var"
  lists <- pVarsList `sepEndBy1` semi 
  return $ concat lists

pVarsList :: Parser [Annotate NameType SrcPos]
pVarsList = do
    pos <- getPosition
    names <- identifier `sepBy` comma
    colon
    tp <- identifier
    return $ map (ret tp pos) names
  where
    ret tp pos name = Annotate (name ::: readType tp) $
      SrcPos {
        srcLine = sourceLine pos,
        srcColumn = sourceColumn pos }

pNameType :: Parser (Annotate NameType SrcPos)
pNameType = withAnnotation $ do
  name <- identifier
  colon
  tp <- identifier
  return $ name ::: readType tp

pFunction :: Parser (Function :~ SrcPos)
pFunction = withAnnotation $ do
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

pProcedure :: Parser (Function :~ SrcPos)
pProcedure = withAnnotation $ do
  reserved "procedure"
  name <- identifier
  args <- parens $ pNameType `sepBy` comma
  semi
  vars <- option [] pVars
  reserved "begin"
  body <- pStatement `sepEndBy1` semi
  reserved "end"
  semi
  return $ Function name args TVoid vars body

pStatement :: Parser (Statement :~ SrcPos)
pStatement =
      try pIfThenElse
  <|> try pAssign
  <|> try pProcedureCall
  <|> try (withAnnotation (reserved "exit" >> return Exit))
  <|> try pReturn
  <|> pFor

pAssign :: Parser (Statement :~ SrcPos)
pAssign = withAnnotation $ do
  var <- identifier
  symbol ":="
  expr <- pExpression
  return $ Assign var expr

pProcedureCall = withAnnotation $ do
  name <- identifier
  args <- parens $ pExpression `sepBy` comma
  return $ Procedure name args

pReturn :: Parser (Statement :~ SrcPos)
pReturn = withAnnotation $ do
  reserved "return"
  x <- pExpression
  return $ Return x

pIfThenElse :: Parser (Statement :~ SrcPos)
pIfThenElse = withAnnotation $ do
  reserved "if"
  cond <- pExpression
  reserved "then"
  ok <- pBlock
  el <- option [] $ try $ do
          reserved "else"
          pBlock
  return $ IfThenElse cond ok el

pBlock = try (one `fmap` pStatement) <|> do
           reserved "begin"
           sts <- pStatement `sepEndBy1` semi
           reserved "end"
--            semi
           return sts
  where
    one x = [x]

pFor = withAnnotation $ do
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
            [binary "^" Pow AssocLeft],
            [binary "*" Mul AssocLeft, binary "/" Div AssocLeft, binary "%" Mod AssocLeft ],
            [binary "+" Add AssocLeft, binary "-" Sub AssocLeft ],
            [binary "=" IsEQ AssocLeft, binary "!=" IsNE AssocLeft, binary ">" IsGT AssocLeft, binary "<" IsLT AssocLeft ]
          ]
    binary  name fun assoc = Infix (op name fun) assoc
    op name fun = do
      pos <- getPosition
      reservedOp name
      return $ \x y -> Annotate (Op fun x y) $ SrcPos {
        srcLine = sourceLine pos,
        srcColumn = sourceColumn pos }

term = parens pExpression
   <|> try (withAnnotation $ Literal `fmap` pLiteral)
   <|> try pCall
   <|> pVariable

pLiteral = try stringLit <|> try intLit <|> boolLit
  where
    stringLit = LString `fmap` P.stringLiteral pascal
    intLit = LInteger `fmap` P.integer pascal 
    boolLit = try (reserved "true" >> return (LBool True)) <|> (reserved "false" >> return (LBool False))

pVariable = withAnnotation $  Variable `fmap` identifier

pCall :: Parser (Expression :~ SrcPos)
pCall = withAnnotation $ do
  name <- identifier
  args <- parens $ pExpression `sepBy` comma
  return $ Call name args

parseSource :: FilePath -> IO (Program :~ SrcPos)
parseSource path = do
  src <- readFile path
  case parse pProgram path src of
    Left err -> fail $ show err
    Right x -> return x
