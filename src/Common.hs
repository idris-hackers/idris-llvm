{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}
module Common ( CodeGen, runCodeGen
              , withVars, lookupVar, updateLocal
              , call, call', idrisCall
              , initialDefs
              , gcAllocBytes, gcAllocValue
              , sizeOf
              , valueTy, bigIntTy
              , constructorTy, primitiveTy
              , isUnboxed
              , targetWordSize, getWordSize
              , ftyToNativeTy
--              , foreignToIdris, idrisToForeign
              ) where

import Control.Applicative
import Control.Monad.State

import Data.Maybe
import Data.Word
import qualified Data.Map as M

import LLVM.General.AST
import LLVM.General.AST.Type
import LLVM.General.AST.Global
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.DataLayout
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.CallingConvention as CallingConvention
import qualified LLVM.General.AST.IntegerPredicate as IPred

import IRTS.Lang (LVar(..), FType(..))
import qualified Idris.Core.TT as TT

import MonadCodeGen
import Util

-- Nothing is an error value, which poisons all code that depends on it
data Env = Env Int [Maybe Operand]

newtype CodeGen a = CodeGen (StateT Env FunctionGen a)
  deriving (Applicative, Functor, Monad, MonadState Env)

instance MonadModuleGen CodeGen where
  getMGS = CodeGen (lift getMGS)
  putMGS = CodeGen . lift . putMGS
  askTarget = CodeGen (lift askTarget)

instance MonadFunctionGen CodeGen where
  getFGS = CodeGen (lift getFGS)
  putFGS = CodeGen . lift . putFGS
  tellFGO = CodeGen . lift . tellFGO

runCodeGen :: CodeGen a -> FunctionGen a
runCodeGen (CodeGen cg) = fst <$> runStateT cg (Env 0 [])

withVars :: [Maybe Operand] -> CodeGen a -> CodeGen a
withVars ops (CodeGen cg) = CodeGen $ do
  let l = length ops
  modify (\(Env el vs) -> Env (el + l) (ops ++ vs))
  r <- cg
  modify (\(Env el vs) -> Env (el - l) (drop l vs))
  return r

lookupVar :: LVar -> CodeGen (Maybe Operand)
lookupVar (Loc level) = gets (\(Env el vs) -> vs !! (el - level))
lookupVar (Glob n) = do
  let name = Name . show $ n
  result <- findGlobal name
  case result of
    Just g -> return $ Just (ConstantOperand . C.GlobalReference (typeOf g) $ name)
    Nothing -> ierror $ "reference to undefined global by name " ++ show n

updateLocal :: Int -> Maybe Operand -> CodeGen ()
updateLocal level v = modify (\(Env el vs) -> Env el (replaceElt (el - level) v vs))

replaceElt :: Int -> a -> [a] -> [a]
replaceElt _ _ [] = ierror "replaceElt: ran out of list"
replaceElt 0 val (_:xs) = val:xs
replaceElt n val (x:xs) = x : replaceElt (n-1) val xs

ierror :: String -> a
ierror msg = error $ "INTERNAL ERROR: CodeGenTypes: " ++ msg

call :: Global -> [Operand] -> CodeGen Operand
call fn@(Function {..}) args = inst $ Call { isTailCall = False
                                           , callingConvention = callingConvention
                                           , returnAttributes = []
                                           , function = Right (ConstantOperand (C.GlobalReference (typeOf fn) name))
                                           , arguments = map (\op -> (op, [])) args
                                           , functionAttributes = []
                                           , metadata = []
                                           }
call _ _ = error "call: Tried to call non-function"

call' :: String -> [Operand] -> CodeGen Operand
call' name args = flip call args =<< (fromJust <$> findGlobal (Name name))

idrisCall :: TT.Name -> [Operand] -> CodeGen Operand
idrisCall name args = do
  let defaults = Call { isTailCall = False
                      , callingConvention = CallingConvention.Fast
                      , returnAttributes = []
                      , function = undefined
                      , arguments = undefined
                      , functionAttributes = []
                      , metadata = []}
  Just fn <- lookupVar (Glob name)
  inst $ defaults { function = Right fn
                  , arguments = map (\x -> (x, [])) args }

initialDefs :: ModuleGen ()
initialDefs = do
  word <- IntegerType <$> getWordSize
  _ <- addType "value" (StructureType False [i32, ArrayType 0 (ptr valueTy)])
  _ <- addType "bigInt" (StructureType False [ IntegerType 32
                                             , IntegerType 32
                                             , ptr word
                                             ])
  _ <- addFunction "GC_init" VoidType [] False functionDefaults
  _ <- addFunction "GC_malloc" (ptr i8) [("bytes", word, [])] False functionDefaults
  _ <- addFunction "GC_malloc_atomic" (ptr i8) [("bytes",  word, [])] False functionDefaults
  return ()

gcAllocBytes :: Operand -> CodeGen Operand
gcAllocBytes size = call' "GC_malloc" [size]

gcAllocBytesAtomic :: Operand -> CodeGen Operand
gcAllocBytesAtomic size = call' "GC_malloc" [size]

gcAllocValue :: Type -> CodeGen Operand
gcAllocValue ty = do
  inst =<< BitCast <$> (gcAllocBytes =<< sizeOf ty) <*> pure (ptr ty) <*> pure []

sizeOf :: Type -> CodeGen Operand
sizeOf ty = ConstantOperand . C.PtrToInt
            (C.GetElementPtr False (C.Null (ptr ty)) [C.Int 32 1]) . IntegerType <$> getWordSize

valueTy :: Type
valueTy = NamedTypeReference (Name "value")

bigIntTy :: Type
bigIntTy = NamedTypeReference (Name "bigInt")

constructorTy :: Word64 -> Type
constructorTy n = StructureType False [i32, ArrayType n (ptr valueTy)]

primitiveTy :: Type -> Type
primitiveTy inner = StructureType False [IntegerType 32, inner]

nullValue :: C.Constant
nullValue = C.Null (PointerType valueTy (AddrSpace 0))

isUnboxed :: Operand -> CodeGen Operand
isUnboxed val = do
  wordSize <- getWordSize
  i <- inst $ PtrToInt val (IntegerType wordSize) []
  flag <- inst $ And i (ConstantOperand (C.Int wordSize 1)) []
  let prefix = case val of
                 LocalReference _ (Name n) -> n ++ "-"
                 _ -> ""
  inst' (prefix ++ "isUnboxed") $ ICmp IPred.NE flag (ConstantOperand (C.Int wordSize 1)) []

targetWordSize :: Target -> Word32
targetWordSize (Target { dataLayout = DataLayout { pointerLayouts = l } }) =
    fst . fromJust $ M.lookup (AddrSpace 0) l

getWordSize :: MonadModuleGen m => m Word32
getWordSize = askTarget >>= return . targetWordSize

ftyToNativeTy :: FType -> Type
ftyToNativeTy (FArith (TT.ATInt TT.ITNative)) = i32
ftyToNativeTy (FArith (TT.ATInt TT.ITBig)) = ptr bigIntTy
ftyToNativeTy (FArith (TT.ATInt (TT.ITFixed ty))) = IntegerType (fromIntegral $ TT.nativeTyWidth ty)
ftyToNativeTy (FArith (TT.ATInt (TT.ITVec e c)))
    = VectorType (fromIntegral c) (IntegerType (fromIntegral $ TT.nativeTyWidth e))
ftyToNativeTy (FArith (TT.ATInt TT.ITChar)) = i32
ftyToNativeTy (FArith TT.ATFloat) = double
ftyToNativeTy FString = ptr i8
ftyToNativeTy FUnit = VoidType
ftyToNativeTy FPtr = ptr i8
ftyToNativeTy FAny = valueTy

-- foreignToIdris :: FType -> Operand -> Codegen Operand
-- foreignToIdris FUnit _ = return $ ConstantOperand nullValue
-- foreignToIdris fty fval = do
--   let ty = primTy (ftyToTy fty)
--   val <- if isHeapFTy fty then alloc ty else allocAtomic ty
--   tagptr <- inst $ GetElementPtr True val [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 0)] []
--   valptr <- inst $ GetElementPtr True val [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 1)] []
--   inst' $ Store False tagptr (ConstantOperand (C.Int 32 (-1))) Nothing 0 []
--   inst' $ Store False valptr fval Nothing 0 []
--   ptrI8 <- inst $ BitCast val (PointerType (IntegerType 8) (AddrSpace 0)) []
--   inst' $ simpleCall "llvm.invariant.start" [ConstantOperand $ C.Int 64 (-1), ptrI8]
--   inst $ BitCast val (PointerType valueType (AddrSpace 0)) []

-- idrisToForeign :: FType -> Operand -> Codegen Operand
-- idrisToForeign FUnit x = return x
-- idrisToForeign fty bval = do
--   val <- inst $ BitCast bval (PointerType (primTy (ftyToTy fty)) (AddrSpace 0)) []
--   fvalptr <- inst $ GetElementPtr True val [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 1)] []
--   inst $ Load False fvalptr Nothing 0 []
