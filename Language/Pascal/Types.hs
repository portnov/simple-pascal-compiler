{-# LANGUAGE RecordWildCards, TypeOperators, StandaloneDeriving, FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving, DeriveDataTypeable, MultiParamTypeClasses, TypeFamilies #-}
module Language.Pascal.Types where

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Exception
import qualified Data.Map as M
import Data.List (intercalate)
import Text.Printf
import Data.Generics hiding (typeOf)

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
    srcPos :: SrcPos
  , typeOf :: Type
  , localSymbols :: M.Map Id Symbol
  , allSymbols :: SymbolTable
  }
  deriving (Eq, Show)

-- | Recursive annotated type
type node :~ ann = Annotate (node ann) ann

-- | Attach type info to node
withType :: Annotate a SrcPos -> Type -> Annotate a TypeAnn
withType (Annotate x pos) t = Annotate x $ TypeAnn {
  srcPos   = pos,
  typeOf   = t,
  localSymbols = M.empty,
  allSymbols = [] }

setType :: Annotate Symbol a -> Type -> Annotate Symbol a
setType (Annotate s pos) t = Annotate (s {symbolType = t}) pos

-- | Change annotation of annotated node
annotate :: ann -> Annotate node old -> Annotate node ann
annotate a (Annotate x _) = Annotate x a

-- | Program
data Program a = Program {
  progConsts :: [(Id, Expression :~ a)], -- ^ constants
  progTypes :: M.Map Id Type,           -- ^ user defined types
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
    symbolName :: Id
  , symbolType :: Type
  , symbolConstValue :: Maybe Lit
  , symbolContext :: Context
  , symbolIndex :: Int   -- ^ Index of symbol in corresponding symbol table
  , symbolDefLine :: Int -- ^ Source line where symbol was defined
  , symbolDefCol :: Int   -- ^ Source column
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

typeOfA ::  Annotate node TypeAnn -> Type
typeOfA = typeOf . annotation

getActualSymbols :: Annotate node TypeAnn -> SymbolTable
getActualSymbols = allSymbols . annotation

lookupSymbol :: Id -> SymbolTable -> Maybe Symbol
lookupSymbol name table =
  msum $ map (M.lookup name) table

-- | Make symbol from it's name and type
(#) :: Id -> Type -> Symbol
name # tp = Symbol {
  symbolName = name,
  symbolType = tp,
  symbolConstValue = Nothing,
  symbolContext = Unknown,
  symbolIndex = 0,
  symbolDefLine = 0,
  symbolDefCol = 0 }

-- | Supported data types
data Type =
    TInteger
  | TString
  | TBool
  | TVoid
  | TUser Id              -- ^ user defined type
  | TAny                  -- ^ any value (dynamic typing)
  | TArray Integer Type   -- ^ array of some type
  | TRecord (Maybe Id) [(Id, Type)]  -- ^ record
  | TField Int Type       -- ^ record field: field index and type
  | TFunction [Type] Type -- ^ formal arguments types and return type
  deriving (Eq, Typeable)

instance Show Type where
  show TInteger = "integer"
  show TString  = "string"
  show TBool    = "boolean"
  show TVoid    = "void"
  show (TUser s) = s
  show TAny     = "any"
  show (TArray sz t) = printf "array [%d] of %s" sz (show t)
  show (TRecord _ pairs) = "record " ++ intercalate ", " (map s pairs) ++ " end"
    where
      s (i,t) = i ++ ": " ++ show t
  show (TField _ t) = "record field of type " ++ show t
  show (TFunction args TVoid) =
    "procedure (" ++ intercalate ", " (map show args) ++ ")"
  show (TFunction args res) =
    "function (" ++ intercalate ", " (map show args) ++ "): " ++ show res

-- | Assignment LHS value: variable or array item
data LValue a =
    LVariable Id
  | LArray Id (Expression :~ a)
  | LField Id Id
  deriving (Eq)

instance Show (LValue a) where
  show (LVariable n) = n
  show (LArray a i) = printf "%s[%s]" a (show i)
  show (LField r f) = r ++ "." ++ f

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
  | RecordField Id Id                            -- ^ record field
  | Literal Lit                                  -- ^ literal value
  | Call Id [Expression :~ a]                    -- ^ functionName(arguments)
  | Op BinOp (Expression :~ a) (Expression :~ a) -- ^ binary operation (x+y etc)
  deriving (Eq)

instance Show (Expression a) where
   show (Variable x) = x
   show (ArrayItem name ix) = printf "%s[%s]" name (show ix)
   show (RecordField name field) = name ++ "." ++ field
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

data Located e = Located ErrorLoc e
  deriving (Eq, Typeable)

instance Show e => Show (Located e) where
  show (Located loc e) =
    printf "At line %d, col. %d:\n\t%s" (errLine loc) (errColumn loc) (show e)

instance Exception e => Exception (Located e)

data ErrorLoc = ErrorLoc {
      errLine :: Int,
      errColumn :: Int,
      errContext :: Context
  } deriving (Eq, Show, Typeable)

nowhere :: ErrorLoc
nowhere = ErrorLoc 0 0 Unknown

data InternalError = InternalError String
  deriving (Eq, Typeable)

instance Show InternalError where
  show (InternalError e) = "Internal compiler error: " ++ e

data TypeError = 
    UnknownSymbol String
  | UnknownType String
  | UnknownConstant String
  | NotConstant String
  | SymbolAlreadyDefined String
  | TypeAlreadyDefined String
  | ConstantAlreadyDefined (String, String)
  | ConstLValue String
  | InvalidOperandTypes (Type, Type)
  | NotAFunction (String, Type)
  | NotAProcedure (String, Type)
  | InvalidFunctionCall ([Type], [Type])
  | NotARecord (String, Type)
  | NoSuchField (String, String)
  | NotAnArray (String, Type)
  | InvalidArrayIndex Type
  | InvalidArrayItemLValue Type
  | InvalidArrayLValue (String, Type)
  | AssignmentTypeMismatch (Type, Type)
  | ReturnTypeMismatch (Type, Type)
  | InvalidConditionType String
  | MissplacedBreak ()
  | MissplacedContinue ()
  | MissplacedExit ()
  | MissplacedReturn String
  | InvalidTypeInLoop (String, String)
  | InternalT InternalError
  deriving (Eq, Show, Typeable)

internalT :: String -> TypeError
internalT msg = InternalT (InternalError msg)

instance Exception TypeError

data GeneratorError = GeneratorError String
  deriving (Eq, Show, Typeable)

instance Exception GeneratorError

-- | Compiler context (where we are?)
data Context =
    Unknown            -- ^ unknown context (== internal error)
  | Outside            -- ^ Outside program body or functions
  | ProgramBody        -- ^ In the program body
  | ForLoop Id Int     -- ^ In the for loop (started on nth instruction, with named counter)
  | InFunction Id Type -- ^ In the named function (returning named type)
  deriving (Eq, Typeable)

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
  userTypes :: M.Map Id Type,
  userConsts :: [(Id, Expression :~ TypeAnn)],
  symbolTable :: SymbolTable,
  contexts :: [Context],
  ckLine :: Int,
  ckColumn :: Int }
  deriving (Eq, Show)

newtype Check e a = Check {runCheck :: EMT e (State CheckState) a}
  deriving (Monad)

instance MonadState CheckState (Check e) where
  get = Check $ lift get
  put = Check . lift . put

class (Monad m) => Checker m where
  type GeneralError m
  enterContext :: Context -> m ()
  dropContext :: m ()
  failCheck :: (a -> GeneralError m) -> a -> m b

inContext :: (Checker m) => Context -> m a -> m a
inContext cxt actions = do
  enterContext cxt
  x <- actions
  dropContext
  return x

