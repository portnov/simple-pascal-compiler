{-# LANGUAGE RecordWildCards, TypeOperators, StandaloneDeriving, FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving #-}
module Language.Pascal.Types where

import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as M
import Data.List (intercalate)
import Text.Printf

import Language.SSVM.Types

-- | Type for symbol identifiers
type Id = String

-- | Attach annotation to node
data Annotate node ann = Annotate {
  content :: node,
  annotation :: ann }
  deriving (Eq)

instance (Show node) => Show (Annotate node ann) where
  show (Annotate x _) = show x

-- | Position of node in the source code
data SrcPos = SrcPos {
  srcLine :: Int,
  srcColumn :: Int }
  deriving (Eq)

instance Show SrcPos where
  show (SrcPos l c) = printf "[l. %d, c. %d]" l c

-- | Node type info
data TypeAnn = TypeAnn {
  srcPos :: SrcPos,
  typeOf :: Type,
  localSymbols :: M.Map Id Symbol }
  deriving (Eq, Show)

-- | Recursive annotated type
type node :~ ann = Annotate (node ann) ann

-- | Attach type info to node
withType :: Annotate a SrcPos -> Type -> Annotate a TypeAnn
withType (Annotate x pos) t = Annotate x $ TypeAnn {
  srcPos   = pos,
  typeOf   = t,
  localSymbols = M.empty}

-- | Change annotation of annotated node
annotate :: ann -> Annotate node old -> Annotate node ann
annotate a (Annotate x _) = Annotate x a

-- | Program
data Program a = Program {
  progVariables :: [Annotate Symbol a], -- ^ global variables
  progFunctions :: [Function :~ a],     -- ^ functions
  progBody :: [Statement :~ a]          -- ^ program body statements
  }        
  deriving (Eq, Show)

-- | Function (or procedure)
data Function a = Function {
  fnName :: String,                    -- ^ function name
  fnFormalArgs :: [Annotate Symbol a], -- ^ formal arguments
  fnResultType :: Type,                -- ^ return type (if TVoid then this is procedure)
  fnVars :: [Annotate Symbol a],       -- ^ local variables
  fnBody :: [Statement :~ a]           -- ^ function body statements
  }
  deriving (Eq, Show)

-- | Symbol table
type SymbolTable = [M.Map Id Symbol]

-- | A symbol
data Symbol = Symbol {
  symbolName :: Id,
  symbolType :: Type,
  symbolDefLine :: Int, -- ^ Source line where symbol was defined
  symbolDefCol :: Int   -- ^ Source column
  }
  deriving (Eq)

instance Show Symbol where
  show (Symbol {..}) = symbolName ++ ": " ++ show symbolType

showSymbol :: Symbol -> String
showSymbol (Symbol {..}) =
  printf "%s: %s (defined at l.%d, c.%d)"
         symbolName (show symbolType) symbolDefLine symbolDefCol

symbolNameC :: Annotate Symbol ann -> Id
symbolNameC = symbolName . content

symbolTypeC :: Annotate Symbol ann -> Type
symbolTypeC = symbolType . content

-- | Make symbol from it's name and type
(#) :: Id -> Type -> Symbol
name # tp = Symbol {
  symbolName = name,
  symbolType = tp,
  symbolDefLine = 0,
  symbolDefCol = 0 }

-- | Supported data types
data Type =
    TInteger
  | TString
  | TBool
  | TVoid
  | TAny                  -- ^ any value (dynamic typing)
  | TArray Integer Type   -- ^ array of some type
  | TFunction [Type] Type -- ^ formal arguments types and return type
  deriving (Eq)

isSubtypeOf :: Type -> Type -> Bool
isSubtypeOf TVoid TVoid = True
isSubtypeOf TVoid _ = False
isSubtypeOf _ TAny = True
isSubtypeOf (TArray _ t1) (TArray _ t2) = t1 `isSubtypeOf` t2
isSubtypeOf (TFunction a1 r1) (TFunction a2 r2) =
  (r1 `isSubtypeOf` r2) && areSubtypesOf a1 a2
isSubtypeOf t1 t2 = t1 == t2

areSubtypesOf :: [Type] -> [Type] -> Bool
areSubtypesOf ts1 ts2 =
  (length ts1 == length ts2) && and (zipWith isSubtypeOf ts1 ts2)

instance Show Type where
  show TInteger = "integer"
  show TString  = "string"
  show TBool    = "boolean"
  show TVoid    = "void"
  show TAny     = "any"
  show (TArray sz t) = printf "array [%d] of %s" sz (show t)
  show (TFunction args TVoid) =
    "procedure (" ++ intercalate ", " (map show args) ++ ")"
  show (TFunction args res) =
    "function (" ++ intercalate ", " (map show args) ++ "): " ++ show res

-- | Assignment LHS value: variable or array item
data LValue a =
    LVariable Id
  | LArray Id (Expression :~ a)
  deriving (Eq)

instance Show (LValue a) where
  show (LVariable n) = n
  show (LArray a i) = printf "%s[%s]" a (show i)

-- | Program statements
data Statement a =
    Assign (LValue :~ a) (Expression :~ a)                         -- ^ lvalue := expression;
  | Procedure Id [Expression :~ a]                                 -- ^ procedureName(arguments);
  | Return (Expression :~ a)                                       -- ^ return expression;
  | Break                                                          -- ^ break (for loop)
  | Continue                                                       -- ^ contnune (for loop)
  | Exit                                                           -- ^ exit (procedure or program)
  | IfThenElse (Expression :~ a) [Statement :~ a] [Statement :~ a] -- ^ if expression then ... else ...
  | For Id (Expression :~ a) (Expression :~ a) [Statement :~ a]    -- ^ for i := start to end do ...
  deriving (Eq)

instance Show (Statement a) where
  show (Assign lvalue expr) = show lvalue ++ " := " ++ show expr ++ ";"
  show (Procedure name args) = name ++ "(" ++ intercalate ", " (map show args) ++ ");"
  show Break = "break;"
  show Continue = "continue;"
  show Exit = "exit;"
  show (Return e) = "return " ++ show e ++ ";"
  show (IfThenElse c a b) = "if " ++ show c ++ " then " ++ show a ++ "else" ++ show b ++ ";"
  show (For name start end body) = "for " ++ name ++ " := " ++ show start ++ " to " ++ show end ++ show body

-- | Literal values
data Lit =
    LInteger Integer
  | LString String
  | LBool Bool
  deriving (Eq)

instance Show Lit where
  show (LInteger i) = show i
  show (LString s)  = s
  show (LBool b)    = show b

-- | Expressions
data Expression a =
    Variable Id                                  -- ^ named variable value
  | ArrayItem Id (Expression :~ a)               -- ^ array item
  | Literal Lit                                  -- ^ literal value
  | Call Id [Expression :~ a]                    -- ^ functionName(arguments)
  | Op BinOp (Expression :~ a) (Expression :~ a) -- ^ binary operation (x+y etc)
  deriving (Eq)

instance Show (Expression a) where
   show (Variable x) = x
   show (ArrayItem name ix) = printf "%s[%s]" name (show ix)
   show (Literal x)  = show x
   show (Call name args) = name ++ "(" ++ intercalate ", " (map show args) ++ ")"
   show (Op op x y) = "(" ++ show x ++ " " ++ show op ++ " " ++ show y ++ ")"

-- | Supported binary operations
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

-- | Code generator state
data CodeGenState = CGState {
  variables :: [Id],           -- ^ declared variables (not used currently)
  currentContext :: [Context], -- ^ current contexts stack
  quoteMode :: Bool,           -- ^ quote (word declaration) mode
  generated :: Code }          -- ^ already generated code
  deriving (Eq, Show)

-- | Starting code generator state
emptyGState :: CodeGenState
emptyGState = CGState {
  variables = [],
  currentContext = [],
  quoteMode = False,
  generated = Code [M.empty] [] }

-- | Compiler error
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

-- | Compiler context (where we are?)
data Context =
    Unknown            -- ^ unknown context (== internal error)
  | Outside            -- ^ Outside program body or functions
  | ProgramBody        -- ^ In the program body
  | ForLoop Id Int     -- ^ In the for loop (started on nth instruction, with named counter)
  | InFunction Id Type -- ^ In the named function (returning named type)
  deriving (Eq)

instance Show Context where
  show Unknown              = "unknown context"
  show Outside              = "outside program body"
  show ProgramBody          = "program body"
  show (ForLoop i _)        = "for loop with counter: " ++ i
  show (InFunction name TVoid) = "procedure " ++ name
  show (InFunction name tp) = printf "function %s(): %s" name (show tp)

-- | Context ID, for labels and variable names generation
contextId :: Context -> String
contextId Unknown             = "unknown"
contextId Outside             = "main"
contextId ProgramBody         = "main"
contextId (ForLoop i n)       = "for_" ++ i ++ "_at_" ++ show n
contextId (InFunction name _) = name

-- | Type checker state
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

