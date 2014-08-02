{-# LANGUAGE RecordWildCards, TypeOperators, TypeSynonymInstances, FlexibleInstances, ViewPatterns, FlexibleContexts, UndecidableInstances, TypeFamilies #-}
module Language.Pascal.TypeCheck
  (checkTypes,
   checkSource
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Exception
import qualified Data.Map as M
import Data.Maybe
import Text.Parsec hiding (State)

import Language.Pascal.Types
import Language.Pascal.Parser

isSubtypeOf :: Type -> Type -> Bool
isSubtypeOf TVoid TVoid = True
isSubtypeOf TVoid _ = False
isSubtypeOf _ TAny = True
isSubtypeOf (TArray _ t1) (TArray _ t2) = t1 `isSubtypeOf` t2
isSubtypeOf t1 (TField _ t2) = t1 `isSubtypeOf` t2
isSubtypeOf (TField _ t1) t2 = t1 `isSubtypeOf` t2
isSubtypeOf (TFunction a1 r1) (TFunction a2 r2) =
  (r1 `isSubtypeOf` r2) && areSubtypesOf a1 a2
isSubtypeOf t1 t2 = t1 == t2

areSubtypesOf :: [Type] -> [Type] -> Bool
areSubtypesOf ts1 ts2 =
  (length ts1 == length ts2) && and (zipWith isSubtypeOf ts1 ts2)

-- | Starting type checker state
emptyState :: M.Map Id Symbol -> CheckState
emptyState builtinSymbols = CheckState {
  userConsts = [],
  userTypes = M.empty,
  symbolTable = [builtinSymbols],
  contexts = [],
  ckLine = 0,
  ckColumn = 0 }

class Typed a where
  typeCheck :: Throws (Located TypeError) e => a :~ SrcPos -> Check e (a :~ TypeAnn)

isFor :: Context -> Bool
isFor (ForLoop _ _) = True
isFor _             = False

returnT :: Throws (Located TypeError) e => Type -> Annotate node1 SrcPos -> node -> Check e (Annotate node TypeAnn)
returnT t x res = do
  syms <- gets symbolTable
  return $ Annotate res $ TypeAnn {
             srcPos = SrcPos {
                        srcLine = srcLine (annotation x),
                        srcColumn = srcColumn (annotation x) },
             typeOf = t,
             localSymbols = M.empty,
             allSymbols = syms }

instance Throws (Located TypeError) e => Checker (Check e) where
  type GeneralError (Check e) = TypeError

  enterContext c = do
    st <- get
    put $ st {contexts = c: contexts st}

  dropContext = do
    st <- get
    case contexts st of
      []  -> failCheck internalT "Internal error in TypeCheck: dropContext on empty context!"
      (_:old) -> put $ st {contexts = old}

  failCheck constructor msg = do
    line <- gets ckLine
    col  <- gets ckColumn
    cxs <- gets contexts
    let loc = ErrorLoc line col (if null cxs
                                   then Unknown
                                   else head cxs)
    Check $ throw $ Located loc $ constructor msg

setPos :: Throws (Located TypeError) e => Annotate a SrcPos -> Check e ()
setPos x = do
  st <- get
  put $ st {ckLine = srcLine (annotation x),
            ckColumn = srcColumn (annotation x)}

errorOnUserTypeSymbol :: Annotate Symbol a -> Annotate Symbol a
errorOnUserTypeSymbol (Annotate (symbolType -> TUser t) _) = error $ "Internal error (symbol): user type: " ++ t
errorOnUserTypeSymbol x = x

checkType :: Throws (Located TypeError) e => Type -> Check e Type
checkType (TArray sz t) = do
    t' <- checkType t
    return (TArray sz t')
checkType (TRecord name pairs) = withSymbolTable $ do
    pairs' <- forM pairs $ \(n,t) -> do
                t' <- checkType t
                addSymbol $ Annotate (n # t') (SrcPos 0 0)
                return (n, t')
    return (TRecord name pairs')
checkType (TUser name) = do
    types <- gets userTypes
    case M.lookup name types of
      Just t -> checkType t
      Nothing -> failCheck UnknownType name
checkType t = return t

checkSymbol :: Throws (Located TypeError) e => Annotate Symbol SrcPos -> Check e (Annotate Symbol TypeAnn)
checkSymbol s = do
  setPos s
  t <- checkType (symbolTypeC s)
  case t of
    TUser name -> failCheck internalT $ "undefined user type: " ++ name
    _ -> do
      let s' = setType s t
      addSymbol s'
      return $ s' `withType` t

getSymbol :: Throws (Located TypeError) e => Id -> Check e Symbol
getSymbol name = do
  table <- gets symbolTable
  case lookupSymbol name table of
    Nothing -> failCheck UnknownSymbol name
    Just s  -> return s

addSymbol :: Throws (Located TypeError) e => Annotate Symbol SrcPos -> Check e ()
addSymbol (Annotate symbol@(Symbol {..}) (SrcPos {..})) = do
  st <- get
  (current:other) <- gets symbolTable
  cx <- gets (head . contexts)
  case M.lookup symbolName current of
    Just s -> failCheck SymbolAlreadyDefined $ showSymbol s
    Nothing -> do
      let newSymbol = symbol {symbolContext = cx,
                              symbolIndex = M.size current,
                              symbolDefLine = srcLine,
                              symbolDefCol = srcColumn}
      let new = M.insert symbolName newSymbol current
      put $ st {symbolTable = (new:other)}

addSymbolTable :: Throws (Located TypeError) e => Check e ()
addSymbolTable = do
  st <- get
  was <- gets symbolTable
  put $ st {symbolTable = (M.empty: was)}

dropSymbolTable :: Throws (Located TypeError) e => Check e ()
dropSymbolTable = do
  st <- get
  was <- gets symbolTable
  case was of
    [] -> failCheck internalT "empty symbol table on dropSymbolTable!"
    (_:older) -> put $ st {symbolTable = older}

withSymbolTable :: Throws (Located TypeError) e => Check e a -> Check e a
withSymbolTable check = do
  addSymbolTable
  x <- check
  dropSymbolTable
  return x

addType :: Throws (Located TypeError) e => Id -> Type -> Check e (Id, Type)
addType name tp = do
  st <- get
  let types = userTypes st
  case M.lookup name types of
    Just _  -> failCheck TypeAlreadyDefined name
    Nothing -> do
      tp' <- checkType tp
      put $ st {userTypes = M.insert name tp' types}
      return (name, tp')

evalConst :: Throws (Located TypeError) e => Expression :~ a -> Check e Lit
evalConst expr = do
    case content expr of
      Variable name -> do
                       consts <- gets userConsts
                       case lookup name consts of
                         Just v -> evalConst v
                         Nothing -> failCheck UnknownConstant name
      Literal v -> return v
      Op op x y -> do
                   x' <- evalConst x
                   y' <- evalConst y
                   return $ eval op x' y'
      x -> failCheck NotConstant (show x)
  where
    eval Add (LInteger x) (LInteger y) = LInteger (x+y)
    eval Sub (LInteger x) (LInteger y) = LInteger (x-y)
    eval Mul (LInteger x) (LInteger y) = LInteger (x*y)
    eval Div (LInteger x) (LInteger y) = LInteger (x `div` y)
    eval Pow (LInteger x) (LInteger y) = error "pow() is not supported yet"
    eval IsGT (LInteger x) (LInteger y) = LBool (x > y)
    eval IsLT (LInteger x) (LInteger y) = LBool (x < y)
    eval IsEQ (LInteger x) (LInteger y) = LBool (x == y)
    eval IsNE (LInteger x) (LInteger y) = LBool (x /= y)
    eval _ _ _ = error "Unsupported operand types in constant expression"

litType :: Lit -> Type
litType (LInteger _) = TInteger
litType (LString _)  = TString
litType (LBool _)    = TBool

addConst :: Throws (Located TypeError) e => Id -> Expression :~ SrcPos -> Check e (Expression :~ TypeAnn)
addConst name e = do
  st <- get
  let consts = userConsts st
  case lookup name consts of
    Just c  -> failCheck ConstantAlreadyDefined (name, show c)
    Nothing -> do
      val <- evalConst e
      syms <- gets symbolTable
      let result = Annotate (Literal val) $ TypeAnn {
                     srcPos = annotation e,
                     typeOf = litType val,
                     localSymbols = M.empty,
                     allSymbols = syms }
      put $ st {userConsts = (name, result): consts}
      return result

instance Typed Program where
  typeCheck p@(content -> Program consts types vars fns body) = withSymbolTable $ do
      setPos p
      consts' <- inContext Outside $
                   forM consts $ \(n,v) -> do
                     v'@(content -> Literal val) <- addConst n v
                     let sym = Annotate {
                                 content = Symbol {
                                   symbolName = n,
                                   symbolType = typeOfA v',
                                   symbolConstValue = Just val,
                                   symbolContext = Outside,
                                   symbolIndex = 0, -- will be re-writen in addSymbol
                                   symbolDefLine = srcLine   (annotation v),
                                   symbolDefCol  = srcColumn (annotation v) },
                                 annotation = annotation v }
                     addSymbol sym
                     return (n, v')
                                    
      types' <- inContext Outside $
                  forM (M.assocs types) $ uncurry addType
      vars' <- inContext Outside $
                 forM vars checkSymbol
      fns'  <- inContext Outside $
                 forM fns $ \fn -> do
                   fn' <- typeCheck fn
                   let f = content fn'
                       tp = TFunction (argTypes f) (fnResultType f)
                       s = SrcPos {
                             srcLine = srcLine (annotation fn),
                             srcColumn = srcColumn (annotation fn) }
                   addSymbol $ Annotate (fnName f # tp) s
                   return fn'
      body' <- inContext ProgramBody $
                 forM body typeCheck
      let program = Program consts' (M.fromList types') (map errorOnUserTypeSymbol vars') fns' body'
      syms <- gets symbolTable
      let newSymbols = makeSymbolTable vars'
      return $ Annotate program $ TypeAnn {
        srcPos = SrcPos 0 0,
        typeOf = TVoid,
        localSymbols = newSymbols,
        allSymbols = newSymbols : syms }
    where
      argTypes :: Function TypeAnn -> [Type]
      argTypes (Function {..}) = map symbolTypeC fnFormalArgs

makeSymbolTable :: [Annotate Symbol TypeAnn] -> M.Map Id Symbol
makeSymbolTable xs = M.fromList $ map pair xs
  where
    pair :: Annotate Symbol TypeAnn -> (Id, Symbol)
    pair (Annotate s (TypeAnn {..})) =
      (symbolName s,
       s { symbolDefLine = srcLine srcPos,
           symbolDefCol  = srcColumn srcPos })

findField :: Id -> [(Id, Type)] -> Maybe (Int, Type)
findField name pairs = go 1 pairs
  where
    go _ [] = Nothing
    go i ((k,v):other)
         | k == name = Just (i, v)
         | otherwise = go (i+1) other

instance Typed LValue where
  typeCheck v@(content -> LVariable name) = do
    setPos v
    sym <- getSymbol name
    returnT (symbolType sym) v (LVariable name)

  typeCheck v@(content -> LArray name ix) = do
    setPos v
    sym <- getSymbol name
    case symbolType sym of
      TArray _ tp -> do
                     ix' <- typeCheck ix
                     when (typeOfA ix' /= TInteger) $
                       failCheck InvalidArrayItemLValue (typeOfA ix')
                     returnT tp v (LArray name ix')
      x -> failCheck InvalidArrayLValue (name, x)

  typeCheck v@(content -> LField base field) = do
    setPos v
    baseSym <- getSymbol base
    case symbolType baseSym of
      TRecord _ pairs -> case findField field pairs of
                           Just (ix,t) -> returnT (TField ix t) v (LField base field)
                           Nothing -> failCheck NoSuchField (base, field)
      x -> failCheck NotARecord (base, x)

instance Typed Statement where
  typeCheck x@(content -> Assign lvalue expr) = do
    setPos x
    lhs <- typeCheck lvalue
    rhs <- typeCheck expr
    let rhsType = typeOfA rhs
        lhsType = typeOfA lhs
    if (rhsType == TAny) || (rhsType `isSubtypeOf` lhsType)
      then do
           let result = Assign lhs rhs
           returnT lhsType x result
      else failCheck AssignmentTypeMismatch (lhsType, rhsType)

  typeCheck s@(content -> Procedure name args) = do
    setPos s
    sym <- getSymbol name
    case symbolType sym of
      TFunction formalArgTypes TVoid -> do
          args' <- mapM typeCheck args
          let actualTypes = map typeOfA args'
          if actualTypes `areSubtypesOf` formalArgTypes
            then returnT TVoid s (Procedure name args')
            else failCheck InvalidFunctionCall (actualTypes, formalArgTypes)
      t -> failCheck NotAProcedure (name, t)

  typeCheck s@(content -> Break) = do
      setPos s
      cxs <- gets contexts
      if null (filter isFor cxs)
        then failCheck MissplacedBreak ()
        else returnT TVoid s Break

  typeCheck s@(content -> Continue) = do
      setPos s
      cxs <- gets contexts
      if null (filter isFor cxs)
        then failCheck MissplacedContinue ()
        else returnT TVoid s Continue

  typeCheck s@(content -> Exit) = do
    setPos s
    cxs <- gets contexts
    case cxs of
      (InFunction _ TVoid:_) -> returnT TVoid s Exit
      (ProgramBody:_)        -> returnT TVoid s Exit
      _                      -> failCheck MissplacedExit ()

  typeCheck s@(content -> Return x) = do
    setPos s
    x' <- typeCheck x
    let retType = typeOfA x'
    cxs <- gets contexts
    case cxs of
      (InFunction _ TVoid:_) -> failCheck MissplacedReturn "return statement in procedure"
      (InFunction _ t:_)
          | retType `isSubtypeOf` t -> returnT (typeOfA x') s (Return x')
          | otherwise -> failCheck ReturnTypeMismatch (t, retType)
      _               -> failCheck MissplacedReturn "return statement not in function"

  typeCheck s@(content -> IfThenElse c a b) = do
    setPos s
    c' <- typeCheck c
    when (typeOfA c' /= TBool) $
      failCheck InvalidConditionType (show c)
    a' <- mapM typeCheck a
    b' <- mapM typeCheck b
    returnT TVoid s (IfThenElse c' a' b')

  typeCheck s@(content -> For name start end body) = inContext (ForLoop name 0) $ do
    setPos s
    sym <- getSymbol name
    when (symbolType sym /= TInteger) $
      failCheck InvalidTypeInLoop ("Counter variable", name)
    start' <- typeCheck start
    when (typeOfA start' /= TInteger) $
      failCheck InvalidTypeInLoop ("Counter start value", show start)
    end' <- typeCheck end
    when (typeOfA end' /= TInteger) $
      failCheck InvalidTypeInLoop ("Counter end value", show end)
    body' <- mapM typeCheck body
    returnT TVoid s (For name start' end' body')

instance Typed Function where
  typeCheck x@(content -> Function {..}) = do
    setPos x
    inContext (InFunction fnName fnResultType) $ withSymbolTable $ do
        args <- mapM checkSymbol fnFormalArgs
        vars <- mapM checkSymbol fnVars
        body <- mapM typeCheck fnBody
        let fn = Function fnName args fnResultType vars body
            tp = TFunction (map typeOfA args) fnResultType
        Annotate result ta <- returnT fnResultType x fn
        let newSymbols = makeSymbolTable vars
        return $ Annotate result $ ta {
                                    localSymbols = newSymbols,
                                    allSymbols = newSymbols : allSymbols ta
                                   }

instance Typed Expression where
  typeCheck e@(content -> Variable x) = do
    setPos e
    sym <- getSymbol x
    returnT (symbolType sym) e (Variable x)

  typeCheck e@(content -> ArrayItem name ix) = do
    setPos e
    sym <- getSymbol name
    case symbolType sym of
      TArray _ tp -> do
          ix' <- typeCheck ix
          when (typeOfA ix' /= TInteger) $
            failCheck InvalidArrayIndex (typeOfA ix')
          returnT tp e (ArrayItem name ix')
      x -> failCheck NotAnArray (name, x)

  typeCheck e@(content -> RecordField base field) = do
    setPos e
    baseSym <- getSymbol base
    case symbolType baseSym of
      TRecord _ pairs -> case findField field pairs of
                           Just (ix,t) -> returnT (TField ix t) e (RecordField base field)
                           Nothing -> failCheck NoSuchField (base, field)
      TField ix t -> returnT (TField ix t) e (RecordField base field)
      x -> failCheck NotARecord (base, x)

  typeCheck e@(content -> Literal x) = returnT (litType x) e (Literal x)

  typeCheck e@(content -> Call name args) = do
    setPos e
    sym <- getSymbol name
    case symbolType sym of
      TFunction formalArgTypes resType -> do
          args' <- mapM typeCheck args
          let actualTypes = map typeOfA args'
          if actualTypes `areSubtypesOf` formalArgTypes
            then returnT resType e (Call name args')
            else failCheck InvalidFunctionCall (actualTypes, formalArgTypes)
      t -> failCheck NotAFunction (name, t)

  typeCheck e@(content -> Op op x y) = do
    setPos e
    x' <- typeCheck x
    y' <- typeCheck y
    let tx = typeOfA x'
        ty = typeOfA y'
    if (TInteger `isSubtypeOf` tx) && (TInteger `isSubtypeOf` ty)
      then if op `elem` [IsEQ, IsNE, IsGT, IsLT]
             then returnT TBool    e (Op op x' y')
             else returnT TInteger e (Op op x' y')
      else failCheck InvalidOperandTypes (tx, ty)

checkTypes :: M.Map Id Symbol -> Program :~ SrcPos -> Program :~ TypeAnn
checkTypes builtinSymbols prog = evalState check (emptyState builtinSymbols)
  where
    check :: State CheckState (Program :~ TypeAnn)
    check = do
      x <- tryEMT (runCheck $ typeCheck prog)
      case x of
        Right result -> return result
        Left  err -> fail $ "type checker: " ++ show err

checkSource :: M.Map Id Symbol -> FilePath -> IO (Program :~ TypeAnn)
checkSource builtinSymbols path = do
  str <- readFile path
  case parse pProgram path str of
    Left err -> fail $ "parser: " ++ show err
    Right prog -> return (checkTypes builtinSymbols prog)

