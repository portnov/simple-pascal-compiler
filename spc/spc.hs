{-# LANGUAGE RecordWildCards, CPP #-}

import Control.Monad (when)
import System.Environment
import System.FilePath
import System.Console.GetOpt
import System.IO

import Language.Pascal.TypeCheck

#ifdef VERSION_simple_pascal_ssvm
import Language.SSVM.Types
import Language.SSVM.Binary
import Language.SSVM.Interpreter

import qualified Language.Pascal.SSVM.CodeGen as SSVM
import qualified Language.Pascal.SSVM.Builtin as SSVMBuiltin
#endif

#ifdef VERSION_simple_pascal_jvm
import System.FilePath
import qualified Data.ByteString.Lazy as B

import qualified JVM.Builder as J
import JVM.ClassFile
import JVM.Converter

import qualified Language.Pascal.JVM.Types as JVMT
import qualified Language.Pascal.JVM.CodeGen as JVM
import qualified Language.Pascal.JVM.Builtin as JVMBuiltin
#endif

data Flag =
    Mode Mode
  | Target Target
  | Output FilePath
  | HelpFlag
  deriving (Eq, Show)

data Mode =
    Compile
  | Assembler
  | Interpret
  deriving (Eq, Show)

data Target = JVM | SSVM
  deriving (Eq, Show)

data Options =
    Help
  | Run {
      mode :: Mode,
      target :: Target,
      inputFile :: FilePath,
      outputFile :: Maybe FilePath }
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Run {
  mode = Compile,
  target = JVM,
  inputFile = "-",
  outputFile = Nothing }

options :: [OptDescr Flag]
options = [
    Option "c" ["compile"]   (NoArg $ Mode Compile)   "compile source code to bytecode"
#ifdef VERSION_simple_pascal_jvm
  , Option "j" ["jvm"]       (NoArg $ Target JVM)     "compile for JVM"
#endif
#ifdef VERSION_simple_pascal_ssvm
  , Option "s" ["ssvm"]      (NoArg $ Target SSVM)    "compile for SSVM"
  , Option "S" []            (NoArg $ Mode Assembler) "dump SSVM assembler code"
  , Option "i" ["interpret"] (NoArg $ Mode Interpret) "compile and interpred compiled bytecode (SSVM target only)"
#endif
  , Option "o" ["output"]    (ReqArg Output "FILE")   "set output file name"
  , Option "h" ["help"]      (NoArg HelpFlag)         "show this help and exit" ]

usage :: String
usage = usageInfo header options
  where
    header = "Usage: spc [TARGET] [MODE] [-o OUTPUT] FILE.pas"

flags2options :: [Flag] -> Options
flags2options = foldl go defaultOptions
  where
    go acc (Mode m)      = acc {mode = m}
    go acc (Output path) = acc {outputFile = Just path}
    go acc (Target tgt)  = acc {target = tgt}
    go _   HelpFlag      = Help

parseCmdLine :: [String] -> Either String Options
parseCmdLine args =
  case getOpt Permute options args of
    (flags, [], []) | HelpFlag `elem` flags -> Right Help
                    | otherwise             -> Left "No input file"
    (flags, [file], []) -> Right (flags2options flags) {inputFile = file}
    (_, (_:_:_), [])    -> Left "More than one input file"
    (_, _, errs)        -> Left $ unlines errs ++ usage

main = do
  args <- getArgs
  case parseCmdLine args of
    Left err -> error err
    Right Help -> putStrLn usage
    Right (Run {..}) -> do
      let ext = case target of
                  JVM -> ".class"
                  SSVM -> ".bytecode"
      let dst = case outputFile of
                  Nothing -> replaceExtension inputFile ext
                  Just x  -> x
      let builtinSymbols = case target of
#ifdef VERSION_simple_pascal_ssvm
                             SSVM -> SSVMBuiltin.builtinSymbols
#endif
#ifdef VERSION_simple_pascal_jvm
                             JVM -> JVMBuiltin.builtinSymbols
#endif
      prog <- checkSource builtinSymbols inputFile
      case target of
#ifdef VERSION_simple_pascal_ssvm
        SSVM -> do
          let codeRev = SSVM.runCodeGen (SSVM.generate prog)
              code = codeRev {cCode = reverse (cCode codeRev)}
          case mode of
            Compile -> dumpCode dst code
            Assembler -> putStrLn $ showCode code
            Interpret -> do
              term <- hIsTerminalDevice stdout
              when term $
                hSetBuffering stdout NoBuffering
              runVM (interpret code)
#endif
#ifdef VERSION_simple_pascal_jvm
        JVM -> do
               let progName = takeBaseName inputFile
               let gen = JVMT.generate prog -- :: JVMT.GenerateJvm e ()
                   jvm = JVM.generateJvm progName gen -- :: J.Generate e ()
                   cls = J.generate [] (JVM.toBS progName) jvm
               B.writeFile dst (encodeClass cls)
#endif
        _ -> fail $ "Unsupported target: " ++ show target


