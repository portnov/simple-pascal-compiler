{-# LANGUAGE RecordWildCards, TypeOperators, StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
module Language.Pascal.Types where

import qualified Data.Map as M
import Data.List (intercalate)
import Text.Printf

type Id = String

data SrcPos a = SrcPos {
  content :: a,
  srcLine :: Int,
  srcColumn :: Int }
  deriving (Eq)

data TypeAnn a = TypeAnn {
  tContent :: a,
  srcPos :: SrcPos a,
  typeOf :: Type,
  localSymbols :: M.Map Id Symbol }
  deriving (Eq)

instance (Show a) => Show (TypeAnn a) where
  show x = show (tContent x)

withType :: SrcPos a -> Type -> TypeAnn a
withType p t = TypeAnn {
  tContent = content p,
  srcPos   = p,
  typeOf   = t,
  localSymbols = M.empty}

type node :~ ann = ann (node ann)

instance (Show a) => Show (SrcPos a) where
  show x = show (content x)

showSrcPos (SrcPos {..}) = printf "[l.%d, c.%d] %s" srcLine srcColumn (show content)

data Program a = Program {
  progVariables :: [a NameType],
  progFunctions :: [Function :~ a],
  progBody :: [Statement :~ a]}

deriving instance (Show (a NameType), Show (Function :~ a), Show (Statement :~ a)) => Show (Program a)
deriving instance (Eq (a NameType), Eq (Function :~ a), Eq (Statement :~ a)) => Eq (Program a)

data Function a = Function {
  fnName :: String,
  fnFormalArgs :: [a NameType],
  fnResultType :: Type,
  fnVars :: [a NameType],
  fnBody :: [Statement :~ a] }

deriving instance (Show (a NameType), Show (Function :~ a), Show (Statement :~ a)) => Show (Function a)
deriving instance (Eq (a NameType), Eq (Function :~ a), Eq (Statement :~ a)) => Eq (Function a)

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
  deriving (Eq, Show)

data Statement a =
    Assign Id (Expression :~ a)
  | Procedure Id [Expression :~ a]
  | Return (Expression :~ a)
  | IfThenElse (Expression :~ a) [Statement :~ a] [Statement :~ a]
  | For Id (Expression :~ a) (Expression :~ a) [Statement :~ a]

deriving instance (Eq (Expression :~ a), Eq (Statement :~ a)) => Eq (Statement a)

instance (Show (Expression :~ a), Show (Statement :~ a)) => Show (Statement a) where
  show (Assign name expr) = name ++ " := " ++ show expr ++ ";"
  show (Procedure name args) = name ++ "(" ++ intercalate ", " (map show args) ++ ");"
  show (Return e) = "return " ++ show e ++ ";"
  show (IfThenElse c a b) = "if " ++ show c ++ " then " ++ show c ++ "else" ++ show b ++ ";"
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
  deriving (Eq)

instance Show BinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show Pow = "^"

