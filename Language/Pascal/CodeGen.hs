{-# LANGUAGE TypeSynonymInstances, TypeOperators, ViewPatterns, FlexibleInstances, RecordWildCards, FlexibleContexts #-}
module Language.Pascal.CodeGen where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.List (intercalate)
import qualified Data.Map as M

import Language.SSVM.Types

import Language.Pascal.Types
import Language.Pascal.Builtin

instance Checker Generate where
  enterContext c = do
    st <- get
    put $ st {currentContext = c: currentContext st}

  dropContext = do
    st <- get
    case currentContext st of
      [] -> failCheck "Internal error: empty context on dropContext!"
      (_:xs) -> put $ st {currentContext = xs}

  failCheck msg = do
    cxs <- gets currentContext
    throwError $ TError {
                  errLine    = 0,
                  errColumn  = 0,
                  errContext = if null cxs
                                 then Unknown
                                 else head cxs,
                  errMessage = msg }

runCodeGen :: Generate () -> Code
runCodeGen gen = generated $ execState go emptyGState
  where
    go :: State CodeGenState ()
    go = do
      x <- runErrorT (runGenerate gen)
      case x of
        Right result -> return result
        Left  err    -> fail $ "code generator: " ++ show err

symbolNameC :: Annotate Symbol ann -> Id
symbolNameC = symbolName . content

getContextString :: Generate String
getContextString = do
    cxs <- gets (map contextId . filter isProgramPart . currentContext)
    return $ intercalate "_" (reverse cxs)
  where
    isProgramPart (ForLoop _ _) = False
    isProgramPart _             = True

setQuoteMode :: Bool -> Generate ()
setQuoteMode b = do
  st <- get
  put $ st {quoteMode = b}

getEndLabel :: Generate String
getEndLabel = do
  cstr <- getContextString
  return $ cstr ++ "__END"

variable :: String -> Generate String
variable seed = do
  st <- get
  cstr <- getContextString
  let name = cstr ++ "_" ++ seed
  put $ st {variables = name: variables st}
  return name

getFullName :: String -> Generate String
getFullName seed = do
  cstr <- getContextString
  return $ cstr ++ "_" ++ seed

label :: String -> Generate String
label seed = do
  st <- get
  cstr <- getContextString
  let n = length $ cCode (generated st)
      name = cstr ++ "_" ++ seed ++ "_at_" ++ show n
      gen = generated st
      (curMarks:oldMarks) = cMarks gen
      marks = M.insert name n curMarks
  put $ st {generated = gen {cMarks = marks:oldMarks}}
  return name

forLoopLabel :: String -> String -> Generate String
forLoopLabel src seed = do
  cxs <- gets currentContext
  case cxs of
    []              -> failCheck "Internal error: forLoopLabel on empty context!"
    (ForLoop _ _:_) -> return $ intercalate "_" (map contextId $ reverse cxs) ++ "_" ++ seed
    _               -> failCheck $ src ++ " not in for loop"

getForCounter :: Generate Id
getForCounter = do
  cxs <- gets currentContext
  case cxs of
    []              -> failCheck "Internal error: getForCounter on empty context!"
    (ForLoop i _:_) -> return i
    _               -> failCheck "Internal error: getForCounter not in for loop!"

labelFrom :: String -> Generate String
labelFrom seed = do
  st <- get
  cstr <- getContextString
  let n = length $ cCode (generated st)
      name = cstr ++ "_" ++ seed ++ "_from_" ++ show n
  return name

putLabelHere :: String -> Generate ()
putLabelHere name = do
  st <- get
  let gen = generated st
      n = length $ cCode (generated st)
      (curMarks:oldMarks) = cMarks gen
      marks = M.insert name n curMarks
  put $ st {generated = gen {cMarks = marks:oldMarks}}

class CodeGen a where
  generate :: a -> Generate ()

instance (CodeGen (a TypeAnn)) => CodeGen (a :~ TypeAnn) where
  generate (content -> x) = generate x

instance CodeGen (Expression TypeAnn) where
  generate (Variable name) = do
    var <- getFullName name
    i (CALL var)
    i READ
  generate (Literal x) =
    case x of
      LInteger n -> push n
      LString s  -> push s
      LBool b    -> push (fromIntegral (fromEnum b) :: Integer)
  generate (Call name args) = do
    forM args generate
    case lookupBuiltin name of
      Just code -> code
      Nothing   -> i (CALL name)
  generate (Op op x y) = do
    case op of
      Mod -> generate x >> generate y
      _   -> generate y >> generate x
    case op of
      Add -> i ADD
      Sub -> i SUB
      Mul -> i MUL
      Div -> i DIV
      Mod -> i REM
      Pow -> failCheck "pow() is not supported yet"
      IsGT -> i CMP
      IsLT -> i CMP >> i NEG
      IsEQ -> i CMP >> i ABS >> push (1 :: Integer) >> i SUB
      IsNE -> i CMP >> i ABS

instance CodeGen (Statement TypeAnn) where
  generate (Assign name expr) = do
    generate expr
    var <- getFullName name
    i (CALL var)
    i ASSIGN
  generate (Procedure name args) = do
    forM args generate
    case lookupBuiltin name of
      Just code -> code
      Nothing   -> i (CALL name)
  generate (Return expr) = do
    generate expr
    end <- getEndLabel
    i (GETMARK end)
    i GOTO
  generate Break = do
    end <- forLoopLabel "break" "endFor"
    i (GETMARK end)
    i GOTO
  generate Continue = do
    start <- forLoopLabel "continue" "forLoop"
    var <- getFullName =<< getForCounter
    i (CALL var)
    i READ
    push (1 :: Integer)
    i ADD
    i (CALL var)
    i ASSIGN
    i (GETMARK start)
    i GOTO
  generate Exit = do
    end <- getEndLabel
    i (GETMARK end)
    i GOTO
  generate (IfThenElse c a b) = do
    generate c
    elseLabel <- labelFrom "else"
    i (GETMARK elseLabel)
    i JZ
    forM a generate
    endIfLabel <- labelFrom "endIf"
    i (GETMARK endIfLabel)
    i GOTO
    putLabelHere elseLabel
    forM b generate
    putLabelHere endIfLabel
  generate (For name start end body) = do
    n <- gets (length . cCode . generated)
    inContext (ForLoop name n) $ do
      generate start
      var <- getFullName name
      i (CALL var)
      i ASSIGN
      loop <- forLoopLabel "for" "forLoop"
      putLabelHere loop
      i (CALL var)
      i READ
      generate end
      i CMP
      endLoop <- forLoopLabel "end for" "endFor"
      i (GETMARK endLoop)
      i JGT
      forM body generate
      i (CALL var)
      i READ
      push (1 :: Integer)
      i ADD
      i (CALL var)
      i ASSIGN
      i (GETMARK loop)
      i GOTO
      putLabelHere endLoop

instance CodeGen (Program TypeAnn) where
  generate (Program {..}) = do
      inContext Outside $ do
          forM progVariables $ \v -> do
            declare (symbolNameC v)
          forM progFunctions $ \fn -> do
            forM (fnFormalArgs $ content fn) $ \a -> do
              i COLON
              push $ (fnName $ content fn) ++ "_" ++ symbolNameC a
              i VARIABLE
            forM (fnVars $ content fn) $ \v -> do
              i COLON
              push $ (fnName $ content fn) ++ "_" ++ symbolNameC v
              i VARIABLE
      forM progFunctions generate
      vars <- gets variables
      inContext Outside $ do
          forM vars declare
      inContext ProgramBody $ do
          forM_ progBody generate
          putLabelHere =<< getEndLabel
    where
      declare name = do
        i COLON
        var <- getFullName name
        push var
        i VARIABLE

instance CodeGen (Function TypeAnn) where
  generate (Function {..}) = do
    i COLON
    push fnName
    setQuoteMode True
    inContext (InFunction fnName fnResultType) $ do
        forM (reverse fnFormalArgs) $ \a -> do
          var <- getFullName (symbolNameC a)
          i (CALL var)
          i ASSIGN
        forM fnBody generate
        putLabelHere =<< getEndLabel
        i NOP
        setQuoteMode False
        i DEFINE

