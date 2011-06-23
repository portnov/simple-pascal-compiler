
import Control.Monad.State
import Text.Printf

import Language.SSVM.Types
import Language.SSVM.Binary

import Language.Pascal.Types
import Language.Pascal.TypeCheck
import Language.Pascal.CodeGen

main = do
  prog <- checkSource "hello.pas"
  let codeRev = generated $ execState (generate prog) emptyGState
      code = codeRev {cCode = reverse (cCode codeRev)}
  putStrLn (showMarks $ head $ cMarks code)
  forM (zip [0..] $ cCode code) $ \(i, x) -> do
    putStrLn $ printf "%d.\t%s" (i :: Int) (show x)
  dumpCode "hello.bytecode" code
--   forM_ (reverse $ cCode $ generated result) $ print
