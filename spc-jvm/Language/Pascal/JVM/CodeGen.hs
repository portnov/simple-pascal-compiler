{-# LANGUAGE TypeSynonymInstances, TypeOperators, ViewPatterns, FlexibleInstances, RecordWildCards, FlexibleContexts, OverlappingInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, DeriveDataTypeable, UndecidableInstances #-}

module Language.Pascal.JVM.CodeGen where 

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Exception
import Data.List (intercalate, findIndex)
import qualified Data.Map as M
import Data.Generics

import Language.Pascal.Types
import JVM.Builder.Monad
import JVM.Builder

data GCState = GCState {
    constants :: [(Id, Lit)]
  , variables :: [Id]
  , currentContext :: [Context]
  } deriving (Eq, Show)

newtype GenerateJvm e a = GenerateJvm {unJvm :: StateT GCState (Generate e) a}
  deriving (Monad, MonadState GCState)

instance GeneratorMonad (GenerateJvm e) where
  getGState = GenerateJvm $ lift get
  putGState = GenerateJvm . lift . put

instance Throws TError e => Checker (GenerateJvm e) where
  enterContext c = do
    st <- get
    put $ st {currentContext = c: currentContext st}

  dropContext = do
    st <- get
    case currentContext st of
      [] -> failCheck "Internal error: empty context on dropContext!"
      (_:xs) -> put $ st {currentContext = xs}

  failCheck msg = do
    cxs <- gets currentContext
    throwError $ TError {
                  errLine    = 0,
                  errColumn  = 0,
                  errContext = if null cxs
                                 then Unknown
                                 else head cxs,
                  errMessage = msg }
