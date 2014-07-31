{-# LANGUAGE RecordWildCards #-}
import Control.Monad (when)
import System.Environment
import System.FilePath
import System.Console.GetOpt
import System.IO

import Language.SSVM.Types
import Language.SSVM.Binary
import Language.SSVM.Interpreter

import Language.Pascal.TypeCheck
import Language.Pascal.SSVM.CodeGen
import Language.Pascal.SSVM.Builtin

data Flag =
    Mode Mode
  | Output FilePath
  | HelpFlag
  deriving (Eq, Show)

data Mode =
    Compile
  | Assembler
  | Interpret
  deriving (Eq, Show)

data Options =
    Help
  | Run {
      mode :: Mode,
      inputFile :: FilePath,
      outputFile :: Maybe FilePath }
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Run {
  mode = Compile,
  inputFile = "-",
  outputFile = Nothing }

options :: [OptDescr Flag]
options = [
  Option "c" ["compile"]   (NoArg $ Mode Compile)   "compile source code to bytecode",
  Option "i" ["interpret"] (NoArg $ Mode Interpret) "compile and interpred compiled bytecode",
  Option "S" []            (NoArg $ Mode Assembler) "dump SSVM assembler code",
  Option "o" ["output"]    (ReqArg Output "FILE")   "set output file name",
  Option "h" ["help"]      (NoArg HelpFlag)         "show this help and exit" ]

usage :: String
usage = usageInfo header options
  where
    header = "Usage: spc [MODE] [-o OUTPUT] FILE.pas"

flags2options :: [Flag] -> Options
flags2options = foldl go defaultOptions
  where
    go acc (Mode m)      = acc {mode = m}
    go acc (Output path) = acc {outputFile = Just path}
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
      let dst = case outputFile of
                  Nothing -> replaceExtension inputFile ".bytecode"
                  Just x  -> x
      prog <- checkSource builtinSymbols inputFile
      let codeRev = runCodeGen (generate prog)
          code = codeRev {cCode = reverse (cCode codeRev)}
      case mode of
        Compile -> dumpCode dst code
        Assembler -> putStrLn $ showCode code
        Interpret -> do
          term <- hIsTerminalDevice stdout
          when term $
            hSetBuffering stdout NoBuffering
          runVM (interpret code)

