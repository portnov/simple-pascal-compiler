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

data JvmType =
    JInteger
  | JReference String
  deriving (Eq, Ord, Show)

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

instructionsByType :: M.Map JvmType TypeInstructions
instructionsByType = M.fromList [
    (JInteger, TypeInstructions ILOAD ISTORE IALOAD IASTORE
                    (Just IADD) (Just ISUB) (Just IMUL) (Just IDIV))
  ]

getInstruction :: Throws (Located GeneratorError) e => String -> JvmType -> (TypeInstructions -> a) -> GenerateJvm e a
getInstruction msg t fn = do
  let msg' = "Unsupported " ++ msg ++ ": " ++ show t
  case M.lookup t instructionsByType of
    Nothing -> failCheck GeneratorError msg
    Just i -> return $ fn i

getInstruction' :: Throws (Located GeneratorError) e => String -> JvmType -> (TypeInstructions -> Maybe a) -> GenerateJvm e a
getInstruction' msg t fn = do
  x <- getInstruction msg t fn
  let msg' = "Unsupported " ++ msg ++ ": " ++ show t
  case x of 
    Nothing -> failCheck GeneratorError msg'
    Just i -> return i

getJvmType :: Type -> JvmType
getJvmType TInteger = JInteger
getJvmType TBool = JInteger
getJvmType (TRecord (Just name) _) = JReference name

toBS :: String -> L.ByteString
toBS str = L.fromStrict $ B.pack $ map (fromIntegral . ord) str

-- data Local = Local Int JvmType
--   deriving (Eq, Show)

-- data Global = Global JvmType
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

-- newLocal :: String -> Type -> GenerateJvm e Local
-- newLocal name t = do
--   let jt = getJvmType t
--   vars <- gets localVariables
--   let idx = M.size vars
--       var = Local idx jt
--       vars' = M.insert name var vars
--   modify $ \st -> st {localVariables = vars'}
--   return var

loadVariable :: Throws (Located GeneratorError) e => String -> SymbolTable -> GenerateJvm e ()
loadVariable name table = do
  case lookupSymbol name table of
    Nothing -> failCheck GeneratorError $ "Unknown variable: " ++ name
    Just symbol ->
      case symbolConstValue symbol of
        Just const -> pushConst const
        Nothing -> if symbolContext symbol == Outside
                     then loadGlobal name (getJvmType $ symbolType symbol)
                     else loadLocal (symbolIndex symbol) (getJvmType $ symbolType symbol)

loadLocal :: Throws (Located GeneratorError) e => Int -> JvmType -> GenerateJvm e ()
loadLocal idx t = do
  instruction <- getInstruction "local variable type" t tiLoad
  prog <- gets programName
  liftG $ 
    J.i0 $ instruction $ fromIntegral idx

loadGlobal :: Throws (Located GeneratorError) e => Id -> JvmType -> GenerateJvm e ()
loadGlobal name t = do
  prog <- gets programName
  liftG $ do
    J.i0 $ ALOAD_ I0
    J.getField (toBS prog) (NameType (toBS name) (getFieldSignature t))

getFieldSignature :: JvmType -> Signature (Field Direct)
getFieldSignature JInteger = IntType
getFieldSignature (JReference t) = ObjectType t

instance CodeGen (Expression :~ TypeAnn) where
  generate e@(content -> Variable name) = do
    consts <- gets constants
    case lookup name consts of
      Just const -> pushConst const
      Nothing -> loadVariable name (getActualSymbols e)

  generate e@(content -> ArrayItem name ix) = do
    generate $ (e {content = Variable name} :: Expression :~ TypeAnn)
    generate ix
    let t = typeOfA e
    instruction <- getInstruction "array type" (getJvmType t) tiLoadArray
    liftG $ J.i0 instruction

  generate e@(content -> Literal x) = pushConst x

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

