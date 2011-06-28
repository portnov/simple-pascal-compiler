
module Language.Pascal.Builtin
  (i, push,
   builtinFunctions,
   lookupBuiltin
  ) where

import Control.Monad.State

import Language.SSVM.Types

import Language.Pascal.Types

-- | Add any stack item to the code
putItem :: StackItem -> Generate ()
putItem x = do
  st <- get
  let gen = generated st
      code = x: cCode (generated st)
  put $ st {generated = gen {cCode = code}}

-- | Generate instruction
i :: Instruction -> Generate ()
i x = do
  q <- gets quoteMode
  if q
    then putItem (Quote $ SInstruction x)
    else putItem (SInstruction x)

-- | Generate PUSH instruction
push :: StackType a => a -> Generate ()
push x = i (PUSH $ toStack x)

-- | List of builtin functions
builtinFunctions :: [(Id, Type, Generate ())]
builtinFunctions =
 [("write",   TFunction [TString] TVoid, write),
  ("writeln", TFunction [TString] TVoid, writeln),
  ("printInt", TFunction [TInteger] TVoid, write),
  ("readln",  TFunction [] TString,      readln),
  ("readInt", TFunction [] TInteger,     readln) ]

-- | If named symbol is builtin, return it's definition
lookupBuiltin :: Id -> Maybe (Generate ())
lookupBuiltin name = look builtinFunctions
  where
    look []                               = Nothing
    look ((s, _, code):other) | s == name = Just code
                              | otherwise = look other

write :: Generate ()
write = i PRINT

writeln :: Generate ()
writeln = do
  i PRINT
  push "\n"
  i PRINT

readln :: Generate ()
readln = i INPUT

