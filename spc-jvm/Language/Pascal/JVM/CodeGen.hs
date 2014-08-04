{-# LANGUAGE TypeSynonymInstances, TypeOperators, ViewPatterns, FlexibleInstances, RecordWildCards, FlexibleContexts, OverlappingInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, DeriveDataTypeable, UndecidableInstances, TypeFamilies, ScopedTypeVariables #-}

module Language.Pascal.JVM.CodeGen where 

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import Data.List (intercalate, findIndex)
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Char (ord)
import Data.Generics
import Data.Word
import Data.Int

import qualified JVM.Builder as J
import JVM.Assembler
import JVM.ClassFile
import JVM.Exceptions
import qualified Java.Lang
import qualified Java.IO

import Language.Pascal.Types
import Language.Pascal.JVM.Types
import Language.Pascal.JVM.Builtin

instructionsByType :: [(FieldType, TypeInstructions)]
instructionsByType = [
    (IntType, TypeInstructions ILOAD ISTORE IALOAD IASTORE
                    IRETURN (Just IF)
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
getJvmType x = error $ "Unsupported type: " ++ show x

toBS :: String -> L.ByteString
toBS str = L.fromStrict $ B.pack $ map (fromIntegral . ord) str

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

newLabel :: GenerateJvm e String
newLabel = do
  last <- gets lastLabel
  modify $ \st -> st {lastLabel = last + 1}
  return $ "dummy__" ++ show last

pushConst :: Lit -> GenerateJvm e ()
pushConst (LInteger i) = liftG $ J.i8 LDC1 (CInteger $ fromIntegral i)
pushConst (LBool b) = liftG $ J.i8 LDC1 (CInteger $ if b then 1 else 0)
pushConst (LString s) = liftG $ J.loadString s

getSymbolType :: Throws (Located GeneratorError) e => Id -> SymbolTable -> GenerateJvm e Type
getSymbolType name table = do
  case lookupSymbol name table of
    Nothing -> failCheck GeneratorError $ "Unknown symbol: " ++ name
    Just symbol -> return $ symbolType symbol

getSymbol :: Throws (Located GeneratorError) e => Id -> SymbolTable -> GenerateJvm e Symbol
getSymbol name table = do
  case lookupSymbol name table of
    Nothing -> failCheck GeneratorError $ "Unknown symbol: " ++ name
    Just symbol -> return symbol

loadVariable :: Throws (Located GeneratorError) e => Id -> SymbolTable -> GenerateJvm e ()
loadVariable name table = do
  symbol <- getSymbol name table
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
    J.i0 $ instruction $ fromIntegral (idx+1)

loadGlobal :: Throws (Located GeneratorError) e => Id -> FieldType -> GenerateJvm e ()
loadGlobal name t = do
  prog <- gets programName
  liftG $ do
    J.i0 $ ALOAD_ I0
    J.getField (toBS prog) (NameType (toBS name) t)

getReturnSignature :: Type -> ReturnSignature
getReturnSignature TVoid = ReturnsVoid
getReturnSignature t = Returns $ getJvmType t

getFunctionSig :: Throws (Located GeneratorError) e => Id -> SymbolTable -> GenerateJvm e MethodSignature
getFunctionSig name table = do
  t <- getSymbolType name table
  case t of
    TFunction argTypes retType ->
        return $ MethodSignature (map getJvmType argTypes)
                                 (Returns $ getJvmType retType)
    _ -> failCheck GeneratorError $ "Invalid function type: " ++ show t

getProcedureSig :: Throws (Located GeneratorError) e => Id -> SymbolTable -> GenerateJvm e MethodSignature
getProcedureSig name table = do
  t <- getSymbolType name table
  case t of
    TFunction argTypes TVoid ->
        return $ MethodSignature (map getJvmType argTypes) ReturnsVoid
    _ -> failCheck GeneratorError $ "Invalid procedure type: " ++ show t

instance (CodeGen a) => CodeGen [a] where
  generate list = forM_ list generate

instance (CodeGen (a TypeAnn)) => CodeGen (a :~ TypeAnn) where
  generate = generate . content

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
    case lookupBuiltin name of
      Nothing -> do
        prog <- gets programName
        liftG $ J.aload_ I0
        generate args
        sig <- getFunctionSig name (getActualSymbols e)
        liftG $ J.invokeVirtual (toBS prog) $ NameType (toBS name) sig
      Just builtin -> builtin args

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

assign :: (Throws (Located GeneratorError) e, CodeGen val) => LValue :~ TypeAnn -> val -> GenerateJvm e ()
assign e@(content -> LVariable name) value = do
  prog <- gets programName
  symbol <- getSymbol name (getActualSymbols e)
  let t = getJvmType $ symbolType symbol
  if symbolContext symbol == Outside
    then do
      liftG $ J.aload_ I0
      generate value
      let nt = NameType (toBS name) t
      liftG $ J.putField (toBS prog) nt
    else do
      generate value
      instruction <- getInstruction "variable type" t tiStore
      liftG $ J.i0 $ instruction (fromIntegral $ symbolIndex symbol + 1)

instance CodeGen (Statement :~ TypeAnn) where
  generate e@(content -> Assign lvalue expr) = do
    assign lvalue expr 

  generate e@(content -> Procedure name args) = do
    case lookupBuiltin name of 
      Nothing -> do
        liftG $ J.aload_ I0
        generate args
        prog <- gets programName
        sig <- getProcedureSig name (getActualSymbols e)
        liftG $ J.invokeVirtual (toBS prog) $ NameType (toBS name) sig
      Just builtin -> builtin args

  generate e@(content -> Return expr) = do
    generate expr
    (InFunction _ retType:_) <- gets currentContext
    let t = getJvmType retType
    instruction <- getInstruction "return value type" t tiReturn
    liftG $ J.i0 instruction

  generate (content -> Exit) = do
    liftG $ J.i0 RETURN

  generate (content -> IfThenElse condition ifStatements elseStatements) = do
    generate condition
    trueLabel <- newLabel
    endIf <- newLabel
    let t = getJvmType (typeOfA condition)
    instruction <- getInstruction' "condition type" t tiIf
    liftG $ instruction C_NE `J.useLabel` trueLabel
    generate elseStatements
    liftG $ GOTO `J.useLabel` endIf
    liftG $ J.setLabel trueLabel
    generate ifStatements
    liftG $ J.setLabel endIf

instance CodeGen (Function TypeAnn) where
  generate (Function {..}) = do
    inContext (InFunction fnName fnResultType) $ do
      let argTypes = map (symbolType . content) fnFormalArgs
          argSignature = map getJvmType argTypes
          retSignature = getReturnSignature fnResultType
      (J.newMethod [ACC_PUBLIC] (toBS fnName) argSignature retSignature $ do
          J.setMaxLocals (fromIntegral $ length fnVars + 1)
          J.setStackSize 20
          generate fnBody
          J.i0 RETURN )
        `catchG`
          (\(e :: UnresolvedLabel) -> fail $ "Internal error: " ++ show e)
    return ()

instance CodeGen (Program :~ TypeAnn) where
  generate (content -> Program {..}) = do
    prog <- gets (toBS . programName)
    inContext Outside $ do
      forM_ progVariables $ \var -> do
        let t = getJvmType (symbolType $ content var)
        liftG $ J.newField [ACC_PUBLIC] (toBS $ symbolName $ content var) t
      forM_ progFunctions $ \fn ->
        generate fn
      init <- (J.newMethod [ACC_PUBLIC] (toBS "<init>") [] ReturnsVoid $ do
                    J.setStackSize 1
                    J.aload_ I0
                    J.invokeSpecial Java.Lang.object Java.Lang.objectInit
                    J.i0 RETURN )
                  `catchG`
                    (\(e :: UnresolvedLabel) -> fail $ "Internal error: " ++ show e)

      realmain <- (J.newMethod [ACC_PUBLIC] (toBS "realmain") [J.arrayOf Java.Lang.stringClass] ReturnsVoid $ do
                    J.setStackSize 20
                    generate progBody
                    J.i0 RETURN)
                  `catchG`
                    (\(e :: UnresolvedLabel) -> fail $ "Internal error: " ++ show e)
      (J.newMethod [ACC_PUBLIC, ACC_STATIC] (toBS "main") [J.arrayOf Java.Lang.stringClass] ReturnsVoid $ do
          J.setStackSize 22
          liftG $ do
            J.new prog
            J.dup
            J.invokeSpecial prog init
            J.aload_ I0
            J.invokeVirtual prog realmain
            J.i0 RETURN )
        `catchG`
          (\(e :: UnresolvedLabel) -> fail $ "Internal error: " ++ show e)
      return ()

