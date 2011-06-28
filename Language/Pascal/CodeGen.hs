{-# LANGUAGE TypeSynonymInstances, TypeOperators, ViewPatterns, FlexibleInstances, RecordWildCards, FlexibleContexts #-}
module Language.Pascal.CodeGen (runCodeGen, CodeGen (..)) where

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

-- | Run code generator
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

-- | Get full name of current context
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

-- | Get label which is at end of current context
-- (program or function)
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

-- | Get full name of variable (with context)
getFullName :: String -> Generate String
getFullName seed = do
  cstr <- getContextString
  return $ cstr ++ "_" ++ seed

-- | Put a label and return it's full name
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

-- | Return full name of label in the for loop.
-- fail if current context is not for loop.
forLoopLabel :: String -> String -> Generate String
forLoopLabel src seed = do
  cxs <- gets currentContext
  case cxs of
    []              -> failCheck "Internal error: forLoopLabel on empty context!"
    (ForLoop _ _:_) -> return $ intercalate "_" (map contextId $ reverse cxs) ++ "_" ++ seed
    _               -> failCheck $ src ++ " not in for loop"

-- | Get name of counter variable of for loop.
-- fail if current context is not for loop.
getForCounter :: Generate Id
getForCounter = do
  cxs <- gets currentContext
  case cxs of
    []              -> failCheck "Internal error: getForCounter on empty context!"
    (ForLoop i _:_) -> return i
    _               -> failCheck "Internal error: getForCounter not in for loop!"

-- | Generate full label name
labelFromHere :: String -> Generate String
labelFromHere seed = do
  st <- get
  cstr <- getContextString
  let n = length $ cCode (generated st)
      name = cstr ++ "_" ++ seed ++ "_from_" ++ show n
  return name

-- | Put label here
putLabelHere :: String -> Generate ()
putLabelHere name = do
  st <- get
  let gen = generated st
      n = length $ cCode (generated st)
      (curMarks:oldMarks) = cMarks gen
      marks = M.insert name n curMarks
  put $ st {generated = gen {cMarks = marks:oldMarks}}

goto :: String -> Generate ()
goto name = jumpWith GOTO name

jumpWith :: Instruction -> String -> Generate ()
jumpWith jump name = do
  i (GETMARK name)
  i jump

assignTo :: Id -> Generate ()
assignTo name = do
  i (CALL name)
  i ASSIGN

readFrom :: Id -> Generate ()
readFrom name = do
  i (CALL name)
  i READ

class CodeGen a where
  generate :: a -> Generate ()

instance (CodeGen (a TypeAnn)) => CodeGen (a :~ TypeAnn) where
  generate = generate . content

instance (CodeGen a) => CodeGen [a] where
  generate list = forM_ list generate

instance CodeGen (Expression TypeAnn) where
  generate (Variable name) =
    readFrom =<< getFullName name

  generate (Literal x) =
    case x of
      LInteger n -> push n
      LString s  -> push s
      LBool b    -> push (fromIntegral (fromEnum b) :: Integer)

  generate (Call name args) = do
    generate args
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
    assignTo =<< getFullName name

  generate (Procedure name args) = do
    generate args
    case lookupBuiltin name of
      Just code -> code
      Nothing   -> i (CALL name)

  generate (Return expr) = do
    generate expr
    goto =<< getEndLabel

  generate Break =
    goto =<< forLoopLabel "break" "endFor"

  generate Continue = do
    start <- forLoopLabel "continue" "forLoop"
    var <- getFullName =<< getForCounter
    -- increment counter
    readFrom var
    push (1 :: Integer)
    i ADD
    assignTo var
    -- go to start of loop
    goto start

  generate Exit =
    -- go to end of procedure or program
    goto =<< getEndLabel

  generate (IfThenElse condition ifStatements elseStatements) = do
    generate condition
    elseLabel <- labelFromHere "else"
    jumpWith JZ elseLabel
    generate ifStatements 
    endIfLabel <- labelFromHere "endIf"
    goto endIfLabel
    putLabelHere elseLabel
    generate elseStatements 
    putLabelHere endIfLabel

  generate (For counter start end body) = do
    -- get current instruction number
    n <- gets (length . cCode . generated)
    inContext (ForLoop counter n) $ do
      -- assign start value to counter
      generate start
      var <- getFullName counter
      assignTo var
      -- loop start label
      loop <- forLoopLabel "for" "forLoop"
      putLabelHere loop
      -- check if counter > end value
      readFrom var
      generate end
      i CMP
      endLoop <- forLoopLabel "end for" "endFor"
      -- jump to end of cycle if it's done
      jumpWith JGT endLoop
      generate body
      -- increment counter
      readFrom var
      push (1 :: Integer)
      i ADD
      assignTo var
      -- go to start of loop
      goto loop
      putLabelHere endLoop

instance CodeGen (Program TypeAnn) where
  generate (Program {..}) = do
      inContext Outside $ do
          -- declare global variables
          forM progVariables $ \v ->
            declare (symbolNameC v)
          -- for all functions, declare their local variables
          -- and arguments
          forM progFunctions $ \fn -> do
            forM (fnFormalArgs $ content fn) $ \a -> do
              i COLON
              push $ (fnName $ content fn) ++ "_" ++ symbolNameC a
              i VARIABLE
            forM (fnVars $ content fn) $ \v -> do
              i COLON
              push $ (fnName $ content fn) ++ "_" ++ symbolNameC v
              i VARIABLE
      -- generate functions
      generate progFunctions
      vars <- gets variables
      inContext Outside $ do
          forM vars declare
      -- generate program body
      inContext ProgramBody $ do
          generate progBody
          putLabelHere =<< getEndLabel
    where
      declare name = do
        i COLON
        push =<< getFullName name
        i VARIABLE

instance CodeGen (Function TypeAnn) where
  generate (Function {..}) = do
    i COLON
    push fnName
    setQuoteMode True
    inContext (InFunction fnName fnResultType) $ do
        -- get actual arguments values from stack
        forM (reverse fnFormalArgs) $ \a ->
          assignTo =<< getFullName (symbolNameC a)
        generate fnBody
        putLabelHere =<< getEndLabel
        i NOP
        setQuoteMode False
        i DEFINE

