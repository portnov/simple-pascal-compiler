{-# LANGUAGE TypeSynonymInstances, TypeOperators, ViewPatterns, FlexibleInstances, RecordWildCards, FlexibleContexts, OverlappingInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, DeriveDataTypeable, UndecidableInstances, TypeFamilies, ScopedTypeVariables #-}

module Language.Pascal.JVM.Types where 

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Data.Word
import Data.Int

import JVM.Assembler
import qualified JVM.Builder as J

import Language.Pascal.Types

data TypeInstructions = TypeInstructions {
    tiLoad :: Word8 -> Instruction
  , tiStore :: Word8 -> Instruction
  , tiLoadArray :: Instruction
  , tiStoreArray :: Instruction
  , tiReturn :: Instruction
  , tiIf :: Maybe (CMP -> Int16 -> Instruction)
  , tiAdd :: Maybe Instruction
  , tiSub :: Maybe Instruction
  , tiMul :: Maybe Instruction
  , tiDiv :: Maybe Instruction
  } 

data GCState = GCState {
    currentContext :: [Context]
  , lastLabel :: Int
  , programName :: String
  } deriving (Eq, Show)

emptyGCState :: String -> GCState
emptyGCState name = GCState [] 0 name

newtype GenerateJvm e a = GenerateJvm {unJvm :: StateT GCState (J.Generate e) a}
  deriving (Monad, MonadState GCState)

liftG :: J.Generate e a -> GenerateJvm e a
liftG run = GenerateJvm $ lift run

catchG :: forall e l a. Exception e => GenerateJvm (Caught e l) a -> (e -> GenerateJvm l a) -> GenerateJvm l a
catchG run handler = do
  st <- get
  let gen = evalStateT (unJvm run) st :: J.Generate (Caught e l) a
  let emt = J.runGenerate gen :: EMT (Caught e l) (State J.GState) a
  gst <- J.getGState
  let caught = emt `catch` (\exc -> J.runGenerate $ evalStateT (unJvm $ handler exc) st)
  GenerateJvm $ lift $ J.Generate caught

instance J.GeneratorMonad (GenerateJvm e) where
  getGState = GenerateJvm $ lift get
  putGState = GenerateJvm . lift . put

instance J.Generator e GenerateJvm where
  throwG e = GenerateJvm $ lift $ J.Generate $ throw e

class CodeGen a where
  generate :: Throws (Located GeneratorError) e => a -> GenerateJvm e ()

