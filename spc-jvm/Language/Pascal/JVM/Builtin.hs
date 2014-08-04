{-# LANGUAGE TypeSynonymInstances, TypeOperators, ViewPatterns, FlexibleInstances, RecordWildCards, FlexibleContexts, OverlappingInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, DeriveDataTypeable, UndecidableInstances, TypeFamilies, ScopedTypeVariables, OverloadedStrings #-}

module Language.Pascal.JVM.Builtin where 

import Control.Monad.Exception
import qualified Data.ByteString as B ()
import qualified Data.Map as M

import qualified JVM.Builder as J
import JVM.Builder.Instructions
import JVM.ClassFile
import qualified Java.Lang
import qualified Java.IO

import Language.Pascal.Types
import Language.Pascal.JVM.Types

-- | List of builtin functions
builtinFunctions :: (CodeGen args, Throws (Located GeneratorError) e) =>
                    [(Id, args -> GenerateJvm e ())]
builtinFunctions =
 [("write",   write),
  ("writeln", writeln),
  ("readln",  readln) ]

builtinTypes :: [(Id, Type)]
builtinTypes =
 [("write",   TFunction [TInteger] TVoid),
  ("writeln", TFunction [TInteger] TVoid),
  ("readln",  TFunction []     TAny) ]

-- | If named symbol is builtin, return it's definition
-- lookupBuiltin :: Id -> Maybe ([Expression :~ TypeAnn] -> GenerateJvm e ())
lookupBuiltin name = lookup name builtinFunctions


readln _ = undefined

write args = do
  liftG $ getStaticField Java.Lang.system Java.IO.out
  generate args
  liftG $ invokeVirtual Java.IO.printStream printInt

writeln args = do
  liftG $ getStaticField Java.Lang.system Java.IO.out
  generate args
  liftG $ invokeVirtual Java.IO.printStream printlnInt

printInt :: NameType (Method Direct)
printInt = NameType "print" $ MethodSignature [IntType] ReturnsVoid

printlnInt :: NameType (Method Direct)
printlnInt = NameType "println" $ MethodSignature [IntType] ReturnsVoid

-- | Symbol table of builtin symbols
builtinSymbols ::  M.Map Id Symbol
builtinSymbols = M.fromList $ map pair builtinTypes
  where
    pair (name, tp) = (name, Symbol {
                                 symbolName = name,
                                 symbolType = tp,
                                 symbolConstValue = Nothing,
                                 symbolContext = Outside,
                                 symbolIndex = 0,
                                 symbolDefLine = 0,
                                 symbolDefCol = 0 })

