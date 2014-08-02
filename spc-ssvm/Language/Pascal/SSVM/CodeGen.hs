{-# LANGUAGE TypeSynonymInstances, TypeOperators, ViewPatterns, FlexibleInstances, RecordWildCards, FlexibleContexts, OverlappingInstances, TypeFamilies, UndecidableInstances #-}
module Language.Pascal.SSVM.CodeGen (runCodeGen, CodeGen (..)) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Data.List (intercalate, findIndex)
import qualified Data.Map as M

import Language.SSVM.Types

import Language.Pascal.Types
import Language.Pascal.SSVM.Types
import Language.Pascal.SSVM.Builtin

instance Throws (Located GeneratorError) e => Checker (Generate e) where
  type GeneralError (Generate e) = GeneratorError

  enterContext c = do
    st <- get
    put $ st {currentContext = c: currentContext st}

  dropContext = do
    st <- get
    case currentContext st of
      [] -> failCheck GeneratorError "Internal error: empty context on dropContext!"
      (_:xs) -> put $ st {currentContext = xs}

  failCheck constructor msg = do
    cxs <- gets currentContext
    let loc = ErrorLoc 0 0 (if null cxs
                              then Unknown
                              else head cxs)
    Generate $ throw $ Located loc $ constructor msg

-- | Run code generator
-- runCodeGen :: Generate e () -> Code
runCodeGen gen = generated $ execState go emptyGState
  where
    go :: State CodeGenState ()
    go = do
      x <- tryEMT (runGenerate gen)
      case x of
        Right result -> return result
        Left  err    -> fail $ "code generator: " ++ show err

-- | Get full name of current context
getContextString :: Generate e String
getContextString = do
    cxs <- gets (map contextId . filter isProgramPart . currentContext)
    return $ intercalate "_" (reverse cxs)
  where
    isProgramPart (ForLoop _ _) = False
    isProgramPart _             = True

setQuoteMode :: Bool -> Generate e ()
setQuoteMode b = do
  st <- get
  put $ st {quoteMode = b}

-- | Get label which is at end of current context
-- (program or function)
getEndLabel :: Generate e String
getEndLabel = do
  cstr <- getContextString
  return $ cstr ++ "__END"

variable :: String -> Generate e String
variable seed = do
  st <- get
  cstr <- getContextString
  let name = cstr ++ "_" ++ seed
  put $ st {variables = name: variables st}
  return name

-- | Get full name of variable (with context)
getFullName :: String -> Generate e String
getFullName seed = do
  cstr <- getContextString
  return $ cstr ++ "_" ++ seed

-- | Put a label and return it's full name
label :: String -> Generate e String
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
forLoopLabel :: Throws (Located GeneratorError) e => String -> String -> Generate e String
forLoopLabel src seed = do
  cxs <- gets currentContext
  case cxs of
    []              -> failCheck GeneratorError "Internal error: forLoopLabel on empty context!"
    (ForLoop _ _:_) -> return $ intercalate "_" (map contextId $ reverse cxs) ++ "_" ++ seed
    _               -> failCheck GeneratorError $ src ++ " not in for loop"

-- | Get name of counter variable of for loop.
-- fail if current context is not for loop.
getForCounter :: Throws (Located GeneratorError) e => Generate e Id
getForCounter = do
  cxs <- gets currentContext
  case cxs of
    []              -> failCheck GeneratorError "Internal error: getForCounter on empty context!"
    (ForLoop i _:_) -> return i
    _               -> failCheck GeneratorError "Internal error: getForCounter not in for loop!"

-- | Generate full label name
labelFromHere :: Throws (Located GeneratorError) e => String -> Generate e String
labelFromHere seed = do
  st <- get
  cstr <- getContextString
  let n = length $ cCode (generated st)
      name = cstr ++ "_" ++ seed ++ "_from_" ++ show n
  return name

-- | Put label here
putLabelHere :: Throws (Located GeneratorError) e => String -> Generate e ()
putLabelHere name = do
  st <- get
  let gen = generated st
      n = length $ cCode (generated st)
      (curMarks:oldMarks) = cMarks gen
      marks = M.insert name n curMarks
  put $ st {generated = gen {cMarks = marks:oldMarks}}

goto :: Throws (Located GeneratorError) e => String -> Generate e ()
goto name = jumpWith GOTO name

jumpWith :: Throws (Located GeneratorError) e => Instruction -> String -> Generate e ()
jumpWith jump name = do
  i (GETMARK name)
  i jump

assignTo :: Throws (Located GeneratorError) e => Id -> Generate e ()
assignTo name = do
  i (CALL name)
  i ASSIGN

readFrom :: Throws (Located GeneratorError) e => Id -> Generate e ()
readFrom name = do
  i (CALL name)
  i READ

findFieldIndex :: Id -> [(Id, Type)] -> Maybe Int
findFieldIndex name pairs =
  (1+) `fmap` findIndex (\p -> fst p == name) pairs

class CodeGen a where
  generate :: Throws (Located GeneratorError) e => a -> Generate e ()

instance (CodeGen (a TypeAnn)) => CodeGen (a :~ TypeAnn) where
  generate = generate . content

instance (CodeGen a) => CodeGen [a] where
  generate list = forM_ list generate

instance CodeGen (Expression :~ TypeAnn) where
  generate e@(content -> RecordField base field) = do
    rec <- getFullName base
    case typeOfA e of
      TRecord _ pairs -> case findFieldIndex field pairs of
                           Just ix -> do
                             i (CALL rec)
                             push ix
                             i READ_ARRAY
                           Nothing -> failCheck GeneratorError $ "Internal error: no such field in " ++ base ++ " record: " ++ field
      TField ix _ -> do
          i (CALL rec)
          push ix
          i READ_ARRAY
      x -> failCheck GeneratorError $ "Internal error: " ++ base ++ " is " ++ show x ++ ", not a Record"

  generate e = generate (content e)

instance CodeGen (Expression TypeAnn) where
  generate (Variable name) = do
    consts <- gets constants
    case lookup name consts of
      Just (LInteger i) -> push i
      Just (LString s)  -> push s
      Just (LBool b)    -> push (fromEnum b)
      Nothing -> readFrom =<< getFullName name

  generate (ArrayItem name ix) = do
    arr <- getFullName name
    i (CALL arr)
    generate ix
    i READ_ARRAY

  generate (RecordField _ _) =
    failCheck GeneratorError "Internal error: RecordField in instance CodeGen (Expression TypeAnn)"

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
    generate x
    generate y
    case op of
      Add -> i ADD
      Sub -> i SUB
      Mul -> i MUL
      Div -> i DIV
      Mod -> i REM
      Pow -> failCheck GeneratorError "pow() is not supported yet"
      IsGT -> i CMP
      IsLT -> i CMP >> i NEG
      IsEQ -> i CMP >> i ABS >> push (1 :: Integer) >> i SUB
      IsNE -> i CMP >> i ABS

instance CodeGen (LValue :~ TypeAnn) where
  generate (content -> LVariable name) =
    assignTo =<< getFullName name

  generate (content -> LArray name ix) = do
    arr <- getFullName name
    i (CALL arr)
    generate ix
    i ASSIGN_ARRAY

  generate v@(content -> LField base field) = do
    var <- getFullName base
    case typeOfA v of
      TRecord _ pairs -> case findFieldIndex field pairs of
                           Just ix -> do
                             i (CALL var)
                             push ix
                             i ASSIGN_ARRAY
                           Nothing -> failCheck GeneratorError $ "Internal error: no such field in " ++ base ++ " record: " ++ field
      TField ix _ -> do
        i (CALL var)
        push ix
        i ASSIGN_ARRAY
      x -> failCheck GeneratorError $ "Internal error: " ++ base ++ " is " ++ show x ++ ", not a Record"

instance CodeGen (Statement TypeAnn) where
  generate (Assign lvalue expr) = do
    generate expr
    generate lvalue

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
          st <- get
          put $ st {constants = map getLit $ reverse progConsts}
          -- declare global variables
          forM progVariables $ \v -> do
            declare (symbolNameC v)
            allocIfNeeded (symbolNameC v) (symbolTypeC v)
          -- for all functions, declare their local variables
          -- and arguments
          forM progFunctions $ \fn -> do
            forM (fnFormalArgs $ content fn) $ \a -> do
              i COLON
              let name = (fnName $ content fn) ++ "_" ++ symbolNameC a
              push name 
              i VARIABLE
              allocIfNeeded' name (symbolTypeC a)
            forM (fnVars $ content fn) $ \v -> do
              i COLON
              let name = (fnName $ content fn) ++ "_" ++ symbolNameC v
              push name 
              i VARIABLE
              allocIfNeeded' name (symbolTypeC v)
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
      getLit (n, (content -> Literal x)) = (n, x)
      getLit (n, x) = error $ "Internal error: not a literal in constant " ++ n ++ ": " ++ show x

      declare name = do
        i COLON
        push =<< getFullName name
        i VARIABLE

      allocIfNeeded' fullName tp =
        case tp of
          TArray sz _ -> do
                         push sz
                         i (CALL fullName)
                         i ARRAY
          TRecord _ pairs -> do
                             push (length pairs)
                             i (CALL fullName)
                             i ARRAY
          _ -> return ()

      allocIfNeeded name tp = do
        fullName <- getFullName name
        allocIfNeeded' fullName tp

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

