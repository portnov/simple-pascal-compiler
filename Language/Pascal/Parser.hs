{-# LANGUAGE RecordWildCards, TypeOperators, StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
module Language.Pascal.Parser (parseSource, pProgram) where

import Control.Applicative ((<$>))
import qualified Data.Map as M
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
                             "return", "if", "then", "else", "for", "to", "do", "of",
                             "exit", "procedure", "break", "continue", "array", "record",
                             "const", "type" ] }

symbol = P.symbol pascal
reserved = P.reserved pascal
reservedOp = P.reservedOp pascal
identifier = P.identifier pascal
stringLiteral = P.stringLiteral pascal
integer = P.integer pascal
semi = P.semi pascal
colon = P.colon pascal
comma = P.comma pascal
dot = P.dot pascal
parens = P.parens pascal
brackets = P.brackets pascal

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
  consts <- option [] pConsts
  types <- M.fromList <$> option [] pTypes
  vars <- option [] pVars
  fns <- many (try pFunction <|> pProcedure)
  reserved "begin"
  sts <- pStatement `sepEndBy1` semi 
  reserved "end"
  dot
  return $ Program consts types vars fns sts

readType str =
  case str of
    "integer" -> TInteger
    "string"  -> TString
    "boolean" -> TBool
    "void"    -> TVoid
    s         -> TUser s

pVars :: Parser [Annotate Symbol SrcPos]
pVars = do
  reserved "var"
  lists <- pVarsList `sepEndBy1` semi 
  return $ concat lists

pTypes :: Parser [(Id, Type)]
pTypes = do
    reserved "type"
    res <- many1 $ do
              name <- identifier
              reservedOp "="
              tp <- pType
              semi
              return (name, content tp)
    return $ map rename res
  where
    rename (name, TRecord _ fs) = (name, TRecord (Just name) fs)
    rename x = x

pConsts :: Parser [(Id, Expression :~ SrcPos)]
pConsts = do
  reserved "const"
  many1 $ do
    name <- identifier
    reservedOp "="
    value <- pExpression
    semi
    return (name, value)

pVarsList :: Parser [Annotate Symbol SrcPos]
pVarsList = do
    pos <- getPosition
    names <- identifier `sepBy` comma
    colon
    tp <- pType
    return $ map (ret tp pos) names
  where
    ret tp pos name = Annotate (name # content tp) $
      SrcPos {
        srcLine = sourceLine pos,
        srcColumn = sourceColumn pos }

pType :: Parser (Annotate Type SrcPos)
pType = try arrayType <|> try recordType <|> simpleType
  where
    arrayType = withAnnotation $ do
      reserved "array"
      sz <- brackets integer
      reserved "of"
      tp <- pType
      return (TArray sz $ content tp)

    recordType = withAnnotation $ do
      reserved "record"
      fields <- field `sepEndBy1` semi
      reserved "end"
      return (TRecord Nothing fields)

    field = do
      name <- identifier
      colon
      tp <- pType
      return (name, content tp)

    simpleType = withAnnotation $ do
      name <- identifier
      return (readType name)

pNameType :: Parser (Annotate Symbol SrcPos)
pNameType = withAnnotation $ do
  name <- identifier
  colon
  tp <- pType
  return $ name # content tp

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
  <|> try (withAnnotation (reserved "break" >> return Break))
  <|> try (withAnnotation (reserved "continue" >> return Continue))
  <|> try (withAnnotation (reserved "exit" >> return Exit))
  <|> try pReturn
  <|> pFor

pAssign :: Parser (Statement :~ SrcPos)
pAssign = withAnnotation $ do
  lv <- pLValue
  symbol ":="
  expr <- pExpression
  return $ Assign lv expr

pLValue :: Parser (LValue :~ SrcPos)
pLValue = try arrayItem <|> try recordField <|> variable
  where
    arrayItem = withAnnotation $ do
      arr <- identifier
      ix <- brackets pExpression
      return (LArray arr ix)

    variable = withAnnotation (LVariable <$> identifier)

    recordField = withAnnotation $ do
      base <- identifier
      dot
      field <- identifier
      return (LField base field)

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

pBlock = try (one <$> pStatement) <|> do
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
   <|> try (withAnnotation $ Literal <$> pLiteral)
   <|> try pCall
   <|> try pArrayItem
   <|> try pRecordField
   <|> pVariable

pLiteral = try stringLit <|> try intLit <|> boolLit
  where
    stringLit = LString <$> stringLiteral
    intLit = LInteger <$> integer
    boolLit = try (reserved "true" >> return (LBool True)) <|> (reserved "false" >> return (LBool False))

pVariable :: Parser (Expression :~ SrcPos)
pVariable = withAnnotation $  Variable <$> identifier

pArrayItem :: Parser (Expression :~ SrcPos)
pArrayItem = withAnnotation $ do
  arr <- identifier
  ix <- brackets pExpression
  return (ArrayItem arr ix)

pRecordField :: Parser (Expression :~ SrcPos)
pRecordField = withAnnotation $ do
  base <- identifier
  dot
  field <- identifier
  return (RecordField base field)

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
