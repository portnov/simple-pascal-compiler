{-# LANGUAGE RecordWildCards, TypeOperators, TypeSynonymInstances, FlexibleInstances, ViewPatterns #-}
module Language.Pascal.TypeCheck where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as M
import Data.Maybe
import Text.Parsec hiding (State)

import Language.Pascal.Types
import Language.Pascal.Builtin
import Language.Pascal.Parser

-- | Look up for named symbol
lookupSymbol :: Id -> SymbolTable -> Maybe Symbol
lookupSymbol name table =
  case filter isJust $ map (M.lookup name) table of
    [] -> Nothing
    (s:_) -> s

-- | Symbol table of builtin symbols
builtinSymbols ::  M.Map Id Symbol
builtinSymbols = M.fromList $ map pair builtinFunctions
  where
    pair (name, tp, _) = (name, Symbol {
                                 symbolName = name,
                                 symbolType = tp,
                                 symbolDefLine = 0,
                                 symbolDefCol = 0 })

-- | Starting type checker state
emptyState :: CheckState
emptyState = CheckState {
  symbolTable = [builtinSymbols],
  contexts = [],
  ckLine = 0,
  ckColumn = 0 }

class Typed a where
  typeCheck :: a :~ SrcPos -> Check (a :~ TypeAnn)

typeOfA ::  Annotate node TypeAnn -> Type
typeOfA = typeOf . annotation

symbolTypeC :: Annotate Symbol ann -> Type
symbolTypeC = symbolType . content

isFor :: Context -> Bool
isFor (ForLoop _ _) = True
isFor _             = False

returnT ::  Type -> Annotate node1 SrcPos -> node -> Check (Annotate node TypeAnn)
returnT t x res =
  return $ Annotate res $ TypeAnn {
             srcPos = SrcPos {
                        srcLine = srcLine (annotation x),
                        srcColumn = srcColumn (annotation x) },
             typeOf = t,
             localSymbols = M.empty}


instance Checker Check where
  enterContext c = do
    st <- get
    put $ st {contexts = c: contexts st}

  dropContext = do
    st <- get
    case contexts st of
      []  -> failCheck "Internal error in TypeCheck: dropContext on empty context!"
      (_:old) -> put $ st {contexts = old}

  failCheck msg = do
    line <- gets ckLine
    col  <- gets ckColumn
    cxs <- gets contexts
    throwError $ TError {
                  errLine    = line,
                  errColumn  = col,
                  errContext = if null cxs
                                 then Unknown
                                 else head cxs,
                  errMessage = msg }


setPos :: Annotate a SrcPos -> Check ()
setPos x = do
  st <- get
  put $ st {ckLine = srcLine (annotation x),
            ckColumn = srcColumn (annotation x)}

getSymbol :: Id -> Check Symbol
getSymbol name = do
  table <- gets symbolTable
  case lookupSymbol name table of
    Nothing -> failCheck $ "Unknown symbol: " ++ name
    Just s  -> return s

addSymbol :: Annotate Symbol SrcPos -> Check ()
addSymbol (Annotate (Symbol {..}) (SrcPos {..})) = do
  st <- get
  (current:other) <- gets symbolTable
  case M.lookup symbolName current of
    Just s -> failCheck $ "Symbol is already defined: " ++ showSymbol s
    Nothing -> do
      let new = M.insert symbolName (Symbol symbolName symbolType srcLine srcColumn) current
      put $ st {symbolTable = (new:other)}

addSymbolTable :: Check ()
addSymbolTable = do
  st <- get
  was <- gets symbolTable
  put $ st {symbolTable = (M.empty: was)}

dropSymbolTable :: Check ()
dropSymbolTable = do
  st <- get
  was <- gets symbolTable
  case was of
    [] -> failCheck "Internal error: empty symbol table on dropSymbolTable!"
    (_:older) -> put $ st {symbolTable = older}

withSymbolTable :: Check a -> Check a
withSymbolTable check = do
  addSymbolTable
  x <- check
  dropSymbolTable
  return x

instance Typed Program where
  typeCheck p@(content -> Program vars fns body) = withSymbolTable $ do
      setPos p
      (vars', fns') <- inContext Outside $ do
          vs <- forM vars $ \v -> do
                   addSymbol v
                   return $ v `withType` symbolTypeC v

          fs <- forM fns $ \fn -> do
                  fn' <- typeCheck fn
                  let f = content fn'
                      tp = TFunction (argTypes f) (fnResultType f)
                      s = SrcPos {
                            srcLine = srcLine (annotation fn),
                            srcColumn = srcColumn (annotation fn) }
                  addSymbol $ Annotate (fnName f # tp) s
                  return fn'
          return (vs, fs)
      body' <- inContext ProgramBody $
                 forM body typeCheck
      let program = Program vars' fns' body'
      return $ Annotate program $ TypeAnn {
        srcPos = SrcPos 0 0,
        typeOf = TVoid,
        localSymbols = makeSymbolTable vars'}
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

instance Typed Statement where
  typeCheck x@(content -> Assign name expr) = do
    setPos x
    sym <- getSymbol name
    rhs <- typeCheck expr
    let rhsType = typeOfA rhs
    if symbolType sym == rhsType
      then do
           let result = Assign name rhs
           returnT rhsType x result
      else failCheck $ "Invalid assignment: LHS type is " ++ show (symbolType sym) ++ ", but RHS type is " ++ show rhsType

  typeCheck s@(content -> Procedure name args) = do
    setPos s
    sym <- getSymbol name
    case symbolType sym of
      TFunction formalArgTypes TVoid -> do
          args' <- mapM typeCheck args
          let actualTypes = map typeOfA args'
          if actualTypes == formalArgTypes
            then returnT TVoid s (Procedure name args')
            else failCheck $ "Invalid types in procedure call: " ++ show actualTypes ++ " instead of " ++ show formalArgTypes
      t -> failCheck $ "Symbol " ++ name ++ " is not a procedure, but " ++ show t

  typeCheck s@(content -> Break) = do
      setPos s
      cxs <- gets contexts
      if null (filter isFor cxs)
        then failCheck "break statement not in for loop"
        else returnT TVoid s Break

  typeCheck s@(content -> Continue) = do
      setPos s
      cxs <- gets contexts
      if null (filter isFor cxs)
        then failCheck "continue statement not in for loop"
        else returnT TVoid s Continue

  typeCheck s@(content -> Exit) = do
    setPos s
    cxs <- gets contexts
    case cxs of
      (InFunction _ TVoid:_) -> returnT TVoid s Exit
      (ProgramBody:_)        -> returnT TVoid s Exit
      _                      -> failCheck "exit statement not in procedure or program body"

  typeCheck s@(content -> Return x) = do
    setPos s
    x' <- typeCheck x
    let retType = typeOfA x'
    cxs <- gets contexts
    case cxs of
      (InFunction _ TVoid:_) -> failCheck "return statement in procedure"
      (InFunction _ t:_)
          | t == retType -> returnT (typeOfA x') s (Return x')
          | otherwise    -> failCheck $ "Return value type does not match: expecting " ++ show t ++ ", got " ++ show retType
      _                  -> failCheck $ "return statement not in function"

  typeCheck s@(content -> IfThenElse c a b) = do
    setPos s
    c' <- typeCheck c
    when (typeOfA c' /= TBool) $
      failCheck $ "Condition type is not Boolean: " ++ show c
    a' <- mapM typeCheck a
    b' <- mapM typeCheck b
    returnT TVoid s (IfThenElse c' a' b')

  typeCheck s@(content -> For name start end body) = inContext (ForLoop name 0) $ do
    setPos s
    sym <- getSymbol name
    when (symbolType sym /= TInteger) $
      failCheck $ "Counter variable is not Integer: " ++ name
    start' <- typeCheck start
    when (typeOfA start' /= TInteger) $
      failCheck $ "Counter start value is not Integer: " ++ show start
    end' <- typeCheck end
    when (typeOfA end' /= TInteger) $
      failCheck $ "Counter end value is not Integer: " ++ show end
    body' <- mapM typeCheck body
    returnT TVoid s (For name start' end' body')

instance Typed Function where
  typeCheck x@(content -> Function {..}) = do
      setPos x
      inContext (InFunction fnName fnResultType) $ withSymbolTable $ do
          args <- mapM varType fnFormalArgs
          vars <- mapM varType fnVars
          body <- mapM typeCheck fnBody
          let fn = Function fnName args fnResultType vars body
              tp = TFunction (map typeOfA args) fnResultType
          Annotate result ta <- returnT fnResultType x fn
          return $ Annotate result $ ta {localSymbols = makeSymbolTable vars}
    where
      varType v = do
        addSymbol v
        return $ v `withType` symbolTypeC v

instance Typed Expression where
  typeCheck e@(content -> Variable x) = do
    setPos e
    sym <- getSymbol x
    returnT (symbolType sym) e (Variable x)

  typeCheck e@(content -> Literal x) = returnT (litType x) e (Literal x)
    where
      litType (LInteger _) = TInteger
      litType (LString _)  = TString
      litType (LBool _)    = TBool

  typeCheck e@(content -> Call name args) = do
    setPos e
    sym <- getSymbol name
    case symbolType sym of
      TFunction formalArgTypes resType -> do
          args' <- mapM typeCheck args
          let actualTypes = map typeOfA args'
          if actualTypes == formalArgTypes
            then returnT resType e (Call name args')
            else failCheck $ "Invalid types in function call: " ++ show actualTypes ++ " instead of " ++ show formalArgTypes
      t -> failCheck $ "Symbol " ++ name ++ " is not a function, but " ++ show t

  typeCheck e@(content -> Op op x y) = do
    setPos e
    x' <- typeCheck x
    y' <- typeCheck y
    case (typeOfA x', typeOfA y') of
      (TInteger, TInteger)
        | op `elem` [IsEQ, IsNE, IsGT, IsLT] -> returnT TBool    e (Op op x' y')
        | otherwise                          -> returnT TInteger e (Op op x' y')
      _ -> failCheck $ "Invalid operand types!"

checkTypes :: Program :~ SrcPos -> Program :~ TypeAnn
checkTypes prog = evalState check emptyState
  where
    check :: State CheckState (Program :~ TypeAnn)
    check = do
      x <- runErrorT (runCheck $ typeCheck prog)
      case x of
        Right result -> return result
        Left  err -> fail $ "type checker: " ++ show err

checkSource :: FilePath -> IO (Program :~ TypeAnn)
checkSource path = do
  str <- readFile path
  case parse pProgram path str of
    Left err -> fail $ "parser: " ++ show err
    Right prog -> return (checkTypes prog)

