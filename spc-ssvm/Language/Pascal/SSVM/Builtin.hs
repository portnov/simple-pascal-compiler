
module Language.Pascal.SSVM.Builtin
  (i, push,
   builtinFunctions,
   builtinSymbols,
   lookupBuiltin
  ) where

import Control.Monad.State
import qualified Data.Map as M

import Language.SSVM.Types

import Language.Pascal.Types
import Language.Pascal.SSVM.Types

-- | Add any stack item to the code
putItem :: StackItem -> Generate e ()
putItem x = do
  st <- get
  let gen = generated st
      code = x: cCode (generated st)
  put $ st {generated = gen {cCode = code}}

-- | Generate instruction
i :: Instruction -> Generate e ()
i x = do
  q <- gets quoteMode
  if q
    then putItem (Quote $ SInstruction x)
    else putItem (SInstruction x)

-- | Generate PUSH instruction
push :: StackType a => a -> Generate e ()
push x = i (PUSH $ toStack x)

-- | List of builtin functions
builtinFunctions :: [(Id, Type, Generate e ())]
builtinFunctions =
 [("write",   TFunction [TAny] TVoid, write),
  ("writeln", TFunction [TAny] TVoid, writeln),
  ("readln",  TFunction []     TAny,  readln) ]

-- | If named symbol is builtin, return it's definition
lookupBuiltin :: Id -> Maybe (Generate e ())
lookupBuiltin name = look builtinFunctions
  where
    look []                               = Nothing
    look ((s, _, code):other) | s == name = Just code
                              | otherwise = look other

write :: Generate e ()
write = i PRINT

writeln :: Generate e ()
writeln = do
  i PRINT
  push "\n"
  i PRINT

readln :: Generate e ()
readln = i INPUT

-- | Symbol table of builtin symbols
builtinSymbols ::  M.Map Id Symbol
builtinSymbols = M.fromList $ map pair builtinFunctions
  where
    pair (name, tp, _) = (name, Symbol {
                                 symbolName = name,
                                 symbolType = tp,
                                 symbolDefLine = 0,
                                 symbolDefCol = 0 })

