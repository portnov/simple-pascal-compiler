
module Language.Pascal.Types where

type Id = String

data Program = Program [NameType] [Function] [Statement]
  deriving (Eq, Show)

data Function = Function {
  fnName :: String,
  fnFormalArgs :: [NameType],
  fnVars :: [NameType],
  fnBody :: [Statement] }
  deriving (Eq, Show)

data NameType = String ::: Type
  deriving (Eq, Show)

data Type =
    TInteger
  | TString
  | TBool
  deriving (Eq, Show)

data Statement =
    Assign Id Expression
  | Procedure Id [Expression]
  | Return Expression
  | IfThenElse Expression [Statement] [Statement]
  | For Id Expression Expression [Statement]
  deriving (Eq, Show)

data Lit =
    LInteger Integer
  | LString String
  | LBool Bool
  deriving (Eq, Show)

data Expression =
    Variable Id
  | Literal Lit
  | Call Id [Expression]
  | Op BinOp Expression Expression
  deriving (Eq, Show)

data BinOp =
    Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  deriving (Eq, Show)

