{-# LANGUAGE RecordWildCards, TypeOperators, StandaloneDeriving, FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving #-}
module Language.Pascal.SSVM.Types where

import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as M
import Data.List (intercalate)
import Text.Printf

import Language.Pascal.Types
import Language.SSVM.Types

-- | Code generator state
data CodeGenState = CGState {
  constants :: [(Id, Lit)],   --
  variables :: [Id],           -- ^ declared variables (not used currently)
  currentContext :: [Context], -- ^ current contexts stack
  quoteMode :: Bool,           -- ^ quote (word declaration) mode
  generated :: Code }          -- ^ already generated code
  deriving (Eq, Show)

-- | Starting code generator state
emptyGState :: CodeGenState
emptyGState = CGState {
  constants = [],
  variables = [],
  currentContext = [],
  quoteMode = False,
  generated = Code [M.empty] [] }

newtype Generate e a = Generate {runGenerate :: EMT e (State CodeGenState) a}
  deriving (Monad, MonadState CodeGenState, MonadError TError)

