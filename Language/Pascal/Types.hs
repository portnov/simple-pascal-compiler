{-# LANGUAGE RecordWildCards #-}
module Language.Pascal.Types where

import Text.Printf
import Data.List (intercalate)

type Id = String

data Ann a = Ann {
  content :: a,
  srcLine :: Int,
  srcColumn :: Int }
  deriving (Eq)

instance (Show a) => Show (Ann a) where
  show x = show (content x)

showAnn (Ann {..}) = printf "[l.%d, c.%d] %s" srcLine srcColumn (show content)

data Program = Program [Ann NameType] [Ann Function] [Ann Statement]
  deriving (Eq, Show)

data Function = Function {
  fnName :: String,
  fnFormalArgs :: [Ann NameType],
  fnVars :: [Ann NameType],
  fnBody :: [Ann Statement] }
  deriving (Eq, Show)

data NameType = String ::: Type
  deriving (Eq, Show)

data Type =
    TInteger
  | TString
  | TBool
  deriving (Eq, Show)

data Statement =
    Assign Id (Ann Expression)
  | Procedure Id [Ann Expression]
  | Return (Ann Expression)
  | IfThenElse (Ann Expression) [Ann Statement] [Ann Statement]
  | For Id (Ann Expression) (Ann Expression) [Ann Statement]
  deriving (Eq)

instance Show Statement where
  show (Assign name expr) = name ++ " := " ++ show expr ++ ";"
  show (Procedure name args) = name ++ "(" ++ intercalate ", " (map show args) ++ ");"
  show (Return e) = "return " ++ show e ++ ";"
  show (IfThenElse c a b) = "if " ++ show c ++ " then " ++ show c ++ "else" ++ show b ++ ";"
  show (For name start end body) = "for " ++ name ++ " := " ++ show start ++ " to " ++ show end ++ show body

data Lit =
    LInteger Integer
  | LString String
  | LBool Bool
  deriving (Eq, Show)

data Expression =
    Variable Id
  | Literal Lit
  | Call Id [Ann Expression]
  | Op BinOp (Ann Expression) (Ann Expression)
  deriving (Eq, Show)

data BinOp =
    Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  deriving (Eq, Show)

