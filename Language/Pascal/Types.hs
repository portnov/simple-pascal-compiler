{-# LANGUAGE RecordWildCards, TypeOperators, StandaloneDeriving, FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving #-}
module Language.Pascal.Types where

import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as M
import Data.List (intercalate)
import Text.Printf

import Language.SSVM.Types

type Id = String

data Annotate node ann = Annotate {
  content :: node,
  annotation :: ann }
  deriving (Eq)

instance (Show node) => Show (Annotate node ann) where
  show (Annotate x _) = show x

data SrcPos = SrcPos {
  srcLine :: Int,
  srcColumn :: Int }
  deriving (Eq)

instance Show SrcPos where
  show (SrcPos l c) = printf "[l. %d, c. %d]" l c

data TypeAnn = TypeAnn {
  srcPos :: SrcPos,
  typeOf :: Type,
  localSymbols :: M.Map Id Symbol }
  deriving (Eq, Show)

type node :~ ann = Annotate (node ann) ann

withType :: Annotate a SrcPos -> Type -> Annotate a TypeAnn
withType (Annotate x pos) t = Annotate x $ TypeAnn {
  srcPos   = pos,
  typeOf   = t,
  localSymbols = M.empty}

annotate :: ann -> Annotate node old -> Annotate node ann
annotate a (Annotate x _) = Annotate x a

data Program a = Program {
  progVariables :: [Annotate NameType a],
  progFunctions :: [Function :~ a],
  progBody :: [Statement :~ a]}

deriving instance (Show a, Show (Function :~ a), Show (Statement :~ a)) => Show (Program a)
deriving instance (Eq a, Eq (Function :~ a), Eq (Statement :~ a)) => Eq (Program a)

data Function a = Function {
  fnName :: String,
  fnFormalArgs :: [Annotate NameType a],
  fnResultType :: Type,
  fnVars :: [Annotate NameType a],
  fnBody :: [Statement :~ a] }

deriving instance (Show a, Show (Function :~ a), Show (Statement :~ a)) => Show (Function a)
deriving instance (Eq a, Eq (Function :~ a), Eq (Statement :~ a)) => Eq (Function a)

data NameType = Id ::: Type
  deriving (Eq, Show)

type SymbolTable = [M.Map Id Symbol]

data Symbol = Symbol {
  symbolName :: Id,
  symbolType :: Type,
  symbolDefLine :: Int,
  symbolDefCol :: Int }
  deriving (Eq)

instance Show Symbol where
  show (Symbol {..}) = symbolName ++ ": " ++ show symbolType

showSymbol (Symbol {..}) =
  printf "%s: %s (defined at l.%d, c.%d)"
         symbolName (show symbolType) symbolDefLine symbolDefCol

data Type =
    TInteger
  | TString
  | TBool
  | TVoid
  | TFunction [Type] Type
  deriving (Eq)

instance Show Type where
  show TInteger = "integer"
  show TString  = "string"
  show TBool    = "boolean"
  show TVoid    = "void"
  show (TFunction args TVoid) =
    "procedure (" ++ intercalate ", " (map show args) ++ ")"
  show (TFunction args res) =
    "function (" ++ intercalate ", " (map show args) ++ "): " ++ show res

data Statement a =
    Assign Id (Expression :~ a)
  | Procedure Id [Expression :~ a]
  | Return (Expression :~ a)
  | Exit
  | IfThenElse (Expression :~ a) [Statement :~ a] [Statement :~ a]
  | For Id (Expression :~ a) (Expression :~ a) [Statement :~ a]

deriving instance (Eq (Expression :~ a), Eq (Statement :~ a)) => Eq (Statement a)

instance (Show (Expression :~ a), Show (Statement :~ a)) => Show (Statement a) where
  show (Assign name expr) = name ++ " := " ++ show expr ++ ";"
  show (Procedure name args) = name ++ "(" ++ intercalate ", " (map show args) ++ ");"
  show Exit = "exit;"
  show (Return e) = "return " ++ show e ++ ";"
  show (IfThenElse c a b) = "if " ++ show c ++ " then " ++ show a ++ "else" ++ show b ++ ";"
  show (For name start end body) = "for " ++ name ++ " := " ++ show start ++ " to " ++ show end ++ show body

data Lit =
    LInteger Integer
  | LString String
  | LBool Bool
  deriving (Eq)

instance Show Lit where
  show (LInteger i) = show i
  show (LString s)  = s
  show (LBool b)    = show b

data Expression a =
    Variable Id
  | Literal Lit
  | Call Id [Expression :~ a]
  | Op BinOp (Expression :~ a) (Expression :~ a)

deriving instance (Eq (Expression :~ a)) => Eq (Expression a)

instance (Show (Expression :~ a)) => Show (Expression a) where
   show (Variable x) = x
   show (Literal x)  = show x
   show (Call name args) = name ++ "(" ++ intercalate ", " (map show args) ++ ")"
   show (Op op x y) = "(" ++ show x ++ " " ++ show op ++ " " ++ show y ++ ")"

data BinOp =
    Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  | IsGT
  | IsLT
  | IsEQ
  | IsNE
  deriving (Eq)

instance Show BinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show Pow = "^"
  show IsGT = ">"
  show IsLT = "<"
  show IsEQ = "="
  show IsNE = "!="

data CodeGenState = CGState {
  variables :: [Id],
  currentContext :: [Context],
  quoteMode :: Bool,
  generated :: Code }
  deriving (Eq, Show)

emptyGState :: CodeGenState
emptyGState = CGState {
  variables = [],
  currentContext = [],
  quoteMode = False,
  generated = Code [M.empty] [] }

data TError = TError {
  errLine :: Int,
  errColumn :: Int,
  errContext :: Context,
  errMessage :: String }
  deriving (Eq)

instance Show TError where
  show (TError {..}) =
    printf "[l.%d, c.%d] (in %s): %s" errLine errColumn (show errContext) errMessage

instance Error TError where
  noMsg = TError 0 0 Unknown "Unknown error"
  strMsg s = TError 0 0 Unknown s

data Context =
    Unknown
  | Outside
  | ProgramBody
  | InFunction Id Type
  deriving (Eq)

instance Show Context where
  show Unknown              = "unknown context"
  show Outside              = "outside program body"
  show ProgramBody          = "program body"
  show (InFunction name TVoid) = "procedure " ++ name
  show (InFunction name tp) = printf "function %s(): %s" name (show tp)

contextId :: Context -> String
contextId Unknown             = "unknown"
contextId Outside             = "global"
contextId ProgramBody         = "main"
contextId (InFunction name _) = name

data CheckState = CheckState {
  symbolTable :: SymbolTable,
  contexts :: [Context],
  ckLine :: Int,
  ckColumn :: Int }
  deriving (Eq, Show)

newtype Generate a = Generate {runGenerate :: ErrorT TError (State CodeGenState) a}
  deriving (Monad, MonadState CodeGenState, MonadError TError)

newtype Check a = Check {runCheck :: ErrorT TError (State CheckState) a}
  deriving (Monad, MonadError TError, MonadState CheckState)

class (Monad m) => Checker m where
  enterContext :: Context -> m ()
  dropContext :: m ()
  failCheck :: String -> m a

inContext :: (Checker m) => Context -> m a -> m a
inContext cxt actions = do
  enterContext cxt
  x <- actions
  dropContext
  return x

