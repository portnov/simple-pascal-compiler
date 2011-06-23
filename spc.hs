
import Control.Monad.State
import System.Environment
import System.FilePath

import Language.SSVM.Types
import Language.SSVM.Binary

import Language.Pascal.Types
import Language.Pascal.TypeCheck
import Language.Pascal.CodeGen

main = do
  args <- getArgs
  case args of
    [path] -> do
      prog <- checkSource path
      let codeRev = generated $ execState (generate prog) emptyGState
          code = codeRev {cCode = reverse (cCode codeRev)}
      dumpCode (replaceExtension path ".bytecode") code
    _ -> putStrLn "Synopsis: spc source.pas"
