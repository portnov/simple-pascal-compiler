{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeOperators #-}

import Control.Monad.Exception
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M

import JVM.ClassFile
import JVM.Converter
import JVM.Assembler
import JVM.Builder hiding (generate)
import qualified JVM.Builder as J
import JVM.Exceptions
import Java.ClassPath

import qualified Java.Lang
import qualified Java.IO

import Language.Pascal.Types
import Language.Pascal.JVM.CodeGen

x :: Symbol
x = Symbol {
      symbolName = "x",
      symbolType = TInteger,
      symbolConstValue = Nothing,
      symbolContext = Outside,
      symbolIndex = 0,
      symbolDefLine = 0,
      symbolDefCol = 0 }

y :: Symbol
y = Symbol {
      symbolName = "y",
      symbolType = TInteger,
      symbolConstValue = Nothing,
      symbolContext = InFunction "test" TVoid,
      symbolIndex = 0,
      symbolDefLine = 0,
      symbolDefCol = 0 }

testType :: TypeAnn 
testType = TypeAnn {
  srcPos = SrcPos 0 0,
  typeOf = TInteger,
  localSymbols = M.empty,
  allSymbols = [M.singleton "y" y, M.singleton "x" x] }

expr :: Expression :~ TypeAnn
expr = Annotate {
         content = Op Add (Annotate (Variable "x") testType)
                          (Annotate (Literal (LInteger 1)) testType),
         annotation = testType
       }

assignment :: Statement :~ TypeAnn
assignment =
  Annotate {
    content = Assign (Annotate (LVariable "y") testType) expr,
    annotation = testType
  }

this :: Id -> Annotate Symbol SrcPos
this name =
  Annotate {
    content = Symbol {
                symbolName = "this",
                symbolType = TRecord (Just name) [],
                symbolConstValue = Nothing,
                symbolContext = Outside,
                symbolIndex = 0,
                symbolDefLine = 0,
                symbolDefCol = 0
              },
    annotation = SrcPos 0 0
  }

test :: (Throws ENotFound e, Throws ENotLoaded e, Throws UnexpectedEndMethod e, Throws (Located GeneratorError) e) => Generate e ()
test = do
  x <- newField [ACC_PUBLIC] "x" IntType
  -- Initializer method. Just calls java.lang.Object.<init>
  init <- newMethod [ACC_PUBLIC] "<init>" [] ReturnsVoid $ do
      setStackSize 4

      aload_ I0
      invokeSpecial Java.Lang.object Java.Lang.objectInit
      aload_ I0
      iconst_2
      putField "Test" x
      i0 RETURN

  testMethod <- newMethod [ACC_PUBLIC] "test" [] ReturnsVoid $ do
    setStackSize 20
    setMaxLocals 3

    generateJvm "Test" $ generate assignment

    getStaticField Java.Lang.system Java.IO.out
    loadString "Result: %d\n"
    iconst_1
    allocArray Java.Lang.object
    dup
    iconst_0
    iload_ I1
    invokeStatic Java.Lang.integer Java.Lang.valueOfInteger
    aastore
    invokeVirtual Java.IO.printStream Java.IO.printf

    i0 RETURN

  newMethod [ACC_PUBLIC, ACC_STATIC] "main" [arrayOf Java.Lang.stringClass] ReturnsVoid $ do
      setStackSize 3

      new "Test"
      dup
      invokeSpecial "Test" init
      invokeVirtual "Test" testMethod
      i0 RETURN

  return ()

main :: IO ()
main = do
  let testClass = J.generate [] "Test" test
  B.writeFile "Test.class" (encodeClass testClass)
