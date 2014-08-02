{-# LANGUAGE TypeSynonymInstances, TypeOperators, ViewPatterns, FlexibleInstances, RecordWildCards, FlexibleContexts, OverlappingInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, DeriveDataTypeable, UndecidableInstances, TypeFamilies #-}

module Language.Pascal.JVM.CodeGen where 

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Exception
import Data.List (intercalate, findIndex)
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Char (ord)
import Data.Generics
import Data.Word

import Language.Pascal.Types
import qualified JVM.Builder as J
import JVM.Assembler
import JVM.ClassFile

data TypeInstructions = TypeInstructions {
    tiLoad :: Word8 -> Instruction
  , tiStore :: Word8 -> Instruction
  , tiLoadArray :: Instruction
  , tiStoreArray :: Instruction
  , tiAdd :: Maybe Instruction
  , tiSub :: Maybe Instruction
  , tiMul :: Maybe Instruction
  , tiDiv :: Maybe Instruction
  } 

instructionsByType :: [(FieldType, TypeInstructions)]
instructionsByType = [
    (IntType, TypeInstructions ILOAD ISTORE IALOAD IASTORE
                    (Just IADD) (Just ISUB) (Just IMUL) (Just IDIV))
  ]

getInstruction :: Throws (Located GeneratorError) e => String -> FieldType -> (TypeInstructions -> a) -> GenerateJvm e a
getInstruction msg t fn = do
  let msg' = "Unsupported " ++ msg ++ ": " ++ show t
  case lookup t instructionsByType of
    Nothing -> failCheck GeneratorError msg'
    Just i -> return $ fn i

getInstruction' :: Throws (Located GeneratorError) e => String -> FieldType -> (TypeInstructions -> Maybe a) -> GenerateJvm e a
getInstruction' msg t fn = do
  x <- getInstruction msg t fn
  let msg' = "Unsupported " ++ msg ++ ": " ++ show t
  case x of 
    Nothing -> failCheck GeneratorError msg'
    Just i -> return i

getJvmType :: Type -> FieldType
getJvmType TInteger = IntType
getJvmType TBool = BoolType
getJvmType (TRecord (Just name) _) = ObjectType name

toBS :: String -> L.ByteString
toBS str = L.fromStrict $ B.pack $ map (fromIntegral . ord) str

-- data Local = Local Int FieldType
--   deriving (Eq, Show)

-- data Global = Global FieldType
--   deriving (Eq, Show)

data GCState = GCState {
    constants :: [(Id, Lit)]
  , currentContext :: [Context]
  , marks :: M.Map String Int
  , programName :: String
  } deriving (Eq, Show)

emptyGCState :: String -> GCState
emptyGCState name = GCState [] [] M.empty name

newtype GenerateJvm e a = GenerateJvm {unJvm :: StateT GCState (J.Generate e) a}
  deriving (Monad, MonadState GCState)

liftG :: J.Generate e a -> GenerateJvm e a
liftG run = GenerateJvm $ lift run

instance J.GeneratorMonad (GenerateJvm e) where
  getGState = GenerateJvm $ lift get
  putGState = GenerateJvm . lift . put

instance Throws (Located GeneratorError) e => Checker (GenerateJvm e) where
  type GeneralError (GenerateJvm e) = GeneratorError

  enterContext c = do
    st <- get
    put $ st {currentContext = c : currentContext st}

  dropContext = do
    st <- get
    case currentContext st of
      [] -> failCheck GeneratorError "Internal error: empty context on dropContext!"
      (_:xs) -> put $ st {currentContext = xs}

  failCheck constructor msg = do
    cxs <- gets currentContext
    let loc = ErrorLoc 0 0 (if null cxs
                              then Unknown
                              else head cxs)
    GenerateJvm $ lift $ J.Generate $ throw $ Located loc $ constructor msg

-- runCodeGen :: GenerateJvm e () -> [Instruction]
runCodeGen name gen = J.generated $ execState go J.emptyGState
  where
    go = do
      x <- tryEMT (J.runGenerate $ runStateT (unJvm gen) (emptyGCState name))
      case x of
        Right result -> return result
        Left  err    -> fail $ "code generator: " ++ show err

generateJvm :: String -> GenerateJvm e () -> J.Generate e ()
generateJvm name gen = evalStateT (unJvm gen) (emptyGCState name)

-- | Get full name of current context
getContextString :: GenerateJvm e String
getContextString = do
    cxs <- gets (map contextId . filter isProgramPart . currentContext)
    return $ intercalate "_" (reverse cxs)
  where
    isProgramPart (ForLoop _ _) = False
    isProgramPart _             = True

pushConst :: Lit -> GenerateJvm e ()
pushConst (LInteger i) = liftG $ J.i8 LDC1 (CInteger $ fromIntegral i)
pushConst (LBool b) = liftG $ J.i8 LDC1 (CInteger $ if b then 1 else 0)
pushConst (LString s) = liftG $ J.loadString s

class CodeGen a where
  generate :: Throws (Located GeneratorError) e => a -> GenerateJvm e ()

getSymbolType :: Throws (Located GeneratorError) e => Id -> SymbolTable -> GenerateJvm e Type
getSymbolType name table = do
  case lookupSymbol name table of
    Nothing -> failCheck GeneratorError $ "Unknown symbol: " ++ name
    Just symbol -> return $ symbolType symbol

loadVariable :: Throws (Located GeneratorError) e => Id -> SymbolTable -> GenerateJvm e ()
loadVariable name table = do
  case lookupSymbol name table of
    Nothing -> failCheck GeneratorError $ "Unknown variable: " ++ name
    Just symbol ->
      case symbolConstValue symbol of
        Just const -> pushConst const
        Nothing -> if symbolContext symbol == Outside
                     then loadGlobal name (getJvmType $ symbolType symbol)
                     else loadLocal (symbolIndex symbol) (getJvmType $ symbolType symbol)

loadLocal :: Throws (Located GeneratorError) e => Int -> FieldType -> GenerateJvm e ()
loadLocal idx t = do
  instruction <- getInstruction "local variable type" t tiLoad
  prog <- gets programName
  liftG $ 
    J.i0 $ instruction $ fromIntegral idx

loadGlobal :: Throws (Located GeneratorError) e => Id -> FieldType -> GenerateJvm e ()
loadGlobal name t = do
  prog <- gets programName
  liftG $ do
    J.i0 $ ALOAD_ I0
    J.getField (toBS prog) (NameType (toBS name) t)

getFunctionSig :: Throws (Located GeneratorError) e => Id -> SymbolTable -> GenerateJvm e MethodSignature
getFunctionSig name table = do
  t <- getSymbolType name table
  case t of
    TFunction argTypes retType ->
        return $ MethodSignature (map getJvmType argTypes)
                                 (Returns $ getJvmType retType)
    _ -> failCheck GeneratorError $ "Invalid function type: " ++ show t

instance CodeGen (Expression :~ TypeAnn) where
  generate e@(content -> Variable name) = do
      loadVariable name (getActualSymbols e)

  generate e@(content -> ArrayItem name ix) = do
    loadVariable name (getActualSymbols e)
    generate ix
    let t = typeOfA e
    instruction <- getInstruction "array type" (getJvmType t) tiLoadArray
    liftG $ J.i0 instruction

  generate e@(content -> RecordField base field) = do
    baseType <- getSymbolType base (getActualSymbols e)
    case baseType of
      TRecord (Just name) fields -> do
        case lookup field fields of
          Nothing -> failCheck GeneratorError $ "Unknown record field: " ++ base ++ "." ++ field
          Just fieldType -> do
              loadVariable base (getActualSymbols e)
              liftG $
                J.getField (toBS base) (NameType (toBS field) (getJvmType fieldType))
      _ -> failCheck GeneratorError $ "Invalid record type: " ++ show baseType

  generate e@(content -> Literal x) = pushConst x

  generate e@(content -> Call name args) = do
    prog <- gets programName
    forM_ args generate
    sig <- getFunctionSig name (getActualSymbols e)
    liftG $ J.invokeVirtual (toBS prog) $ NameType (toBS name) sig

  generate e@(content -> Op op x y) = do
    generate x
    generate y
    let fn = case op of
               Add -> tiAdd
               Sub -> tiSub
               Mul -> tiMul
               Div -> tiDiv
    let t = getJvmType (typeOfA e)
    instruction <- getInstruction' "expression type" t fn
    liftG $ J.i0 instruction

