
import Control.Monad.State

import Language.SSVM.Types
import Language.SSVM.Binary

import Language.Pascal.Types
import Language.Pascal.TypeCheck
import Language.Pascal.CodeGen

main = do
  prog <- checkSource "hello.pas"
  let codeRev = generated $ execState (generate prog) emptyGState
      code = codeRev {cCode = reverse (cCode codeRev)}
  print code
  dumpCode "hello.bytecode" code
--   forM_ (reverse $ cCode $ generated result) $ print
