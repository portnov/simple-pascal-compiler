
module Language.Pascal.Builtin where

import Control.Monad.State

import Language.SSVM.Types

import Language.Pascal.Types

putItem :: StackItem -> Generate ()
putItem x = do
  st <- get
  let gen = generated st
      code = x: cCode (generated st)
  put $ st {generated = gen {cCode = code}}

i :: Instruction -> Generate ()
i x = do
  q <- gets quoteMode
  if q
    then putItem (Quote $ SInstruction x)
    else putItem (SInstruction x)

push :: StackType a => a -> Generate ()
push x = i (PUSH $ toStack x)

builtinFunctions :: [(Id, Type, Generate ())]
builtinFunctions =
 [("write",   TFunction [TString] TVoid, write),
  ("writeln", TFunction [TString] TVoid, writeln),
  ("printInt", TFunction [TInteger] TVoid, printInt),
  ("readln",  TFunction [] TInteger,     readln) ]

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

printInt :: Generate ()
printInt = writeln

readln :: Generate ()
readln = i INPUT

