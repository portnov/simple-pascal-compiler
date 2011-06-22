{-# LANGUAGE TypeSynonymInstances, TypeOperators, ViewPatterns, FlexibleInstances, RecordWildCards #-}
module Language.Pascal.CodeGen where

import Control.Monad
import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map as M

import Language.SSVM.Types
import Language.SSVM.Binary

import Language.Pascal.Types
import Language.Pascal.TypeCheck

type Context = [Id]

data CodeGenState = CGState {
  variables :: [Id],
  currentContext :: Context,
  generated :: Code }
  deriving (Eq, Show)

emptyGState :: CodeGenState
emptyGState = CGState {
  variables = [],
  currentContext = [],
  generated = Code M.empty [] }

type Generate a = State CodeGenState a

putItem :: StackItem -> Generate ()
putItem x = do
  st <- get
  let gen = generated st
      code = x: cCode (generated st)
  put $ st {generated = gen {cCode = code}}

i :: Instruction -> Generate ()
i x = putItem (SInstruction x)

push :: StackType a => a -> Generate ()
push x = i (PUSH $ toStack x)

enterContext :: Id -> Generate ()
enterContext name = do
  st <- get
  put $ st {currentContext = name: currentContext st}

exitContext :: Generate ()
exitContext = do
  st <- get
  case currentContext st of
    [] -> fail "Internal error: empty context on exitContext!"
    (x:xs) -> put $ st {currentContext = xs}

getEndLabel :: Generate String
getEndLabel = do
  context <- gets currentContext
  let name = intercalate "_" context ++ "__END"
  return name

variable :: String -> Generate String
variable seed = do
  st <- get
  let name = intercalate "_" (currentContext st) ++ "_" ++ seed
  put $ st {variables = name: variables st}
  return name

getFullName :: String -> Generate String
getFullName seed = do
  st <- get
  let name = intercalate "_" (currentContext st) ++ "_" ++ seed
  return name

label :: String -> Generate String
label seed = do
  st <- get
  let n = length $ cCode (generated st)
      name = intercalate "_" (currentContext st) ++ "_" ++ seed ++ "_at_" ++ show n
      gen = generated st
      marks = M.insert name n (cMarks gen)
  put $ st {generated = gen {cMarks = marks}}
  return name

labelFrom :: String -> Generate String
labelFrom seed = do
  st <- get
  let n = length $ cCode (generated st)
      name = intercalate "_" (currentContext st) ++ "_" ++ seed ++ "_from_" ++ show n
  return name

putLabelHere :: String -> Generate ()
putLabelHere name = do
  st <- get
  let gen = generated st
      n = length $ cCode (generated st)
      marks = M.insert name n (cMarks gen)
  put $ st {generated = gen {cMarks = marks}}

class CodeGen a where
  generate :: a -> Generate ()

instance CodeGen (Expression :~ TypeAnn) where
  generate (tContent -> Variable name) = do
    var <- getFullName name
    i (CALL var)
    i READ
  generate (tContent -> Literal x) =
    case x of
      LInteger n -> push n
      LString s  -> push s
      LBool b    -> push (fromIntegral (fromEnum b) :: Integer)
  generate (tContent -> Call name args) = do
    forM args generate
    i (CALL name)
  generate (tContent -> Op op x y) = do
    generate y
    generate x
    case op of
      Add -> i ADD
      Sub -> i SUB
      Mul -> i MUL
      Div -> i DIV
      Mod -> i REM
      Pow -> fail "pow() is not supported yet"

instance CodeGen (Statement :~ TypeAnn) where
  generate (tContent -> Assign name expr) = do
    generate expr
    var <- getFullName name
    i (CALL var)
    i ASSIGN
  generate (tContent -> Procedure name args) = do
    forM args generate
    case name of
      "writeln" -> i PRINT >> push "\n" >> i PRINT
      _         -> i (CALL name)
  generate (tContent -> Return expr) = do
    generate expr
    end <- getEndLabel
    i (GETMARK end)
    i GOTO
  generate (tContent -> IfThenElse c a b) = do
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
  generate (tContent -> For name start end body) = do
    generate start
    var <- getFullName name
    i (CALL var)
    i ASSIGN
    loop <- label "forLoop"
    i (CALL var)
    i READ
    generate end
    i CMP
    endLoop <- labelFrom "endFor"
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

instance CodeGen (Program :~ TypeAnn) where
  generate (tContent -> Program {..}) = do
    forM progVariables $ \v -> do
      i COLON
      let (name ::: tp) = tContent v
      var <- getFullName name
      push var
      i VARIABLE
    -- FIXME: generate functions
    forM_ progBody generate

    
