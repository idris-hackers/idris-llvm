{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP, OverloadedStrings #-}
module IRTS.CodegenLLVM (codegenLLVM) where

import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import IRTS.System

import qualified Idris.Core.TT as TT
import Idris.Core.TT (ArithTy(..), IntTy(..), NativeTy(..), nativeTyWidth, sUN)

import Util.System
import Paths_idris_llvm
import System.FilePath

import qualified LLVM
import LLVM.Context
import qualified LLVM.Target as Target
import LLVM.AST
import LLVM.AST.AddrSpace
import LLVM.AST.DataLayout
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.FloatingPointPredicate as FPred
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
import Data.Char
import Data.List
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Word
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V
import Control.Applicative
import Control.Monad.RWS
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Error

import qualified System.Info as SI (arch, os)
import System.IO
import System.Directory (removeFile)
import System.Environment
import System.FilePath ((</>))
import System.Process (rawSystem)
import System.Exit (ExitCode(..))
import Debug.Trace

data Target = Target { triple :: String, dataLayout :: DataLayout }
type Machine = String
-- These might want to live in a different file


-- TODO These should probably be derived from the triple
#if defined(FREEBSD) || defined(DRAGONFLY)
extraLib = ["-L/usr/local/lib"]
#else
extraLib = []
#endif

getRtsDir = do dir <- getDataDir
               return $ addTrailingPathSeparator dir

#if defined(FREEBSD) || defined(DRAGONFLY)
extraInclude = " -I/usr/local/include"
#else
extraInclude = ""
#endif

-- This is dumb, but needed.
fromSBS :: BS.ShortByteString -> String
fromSBS = T.unpack . decodeUtf8 . BS.fromShort 


getIncFlags = do dir <- getDataDir
                 return $ ["-I" ++ dir </> "rts", extraInclude]


-- FIXME: 'optimisation' is no longer a field in CodeGenerator, because
-- optimisation levels at the idris command line are meant to
-- indicate idris optimisation levels, not levels to pass through to
-- back ends. We may need a way to pass back end specific options through.
codegenLLVM :: CodeGenerator
codegenLLVM ci = codegenLLVM' (simpleDecls ci) (targetTriple ci)
                              (targetCPU ci) 2
                              (outputFile ci) (outputType ci)

codegenLLVM' :: [(TT.Name, SDecl)] ->
                String -> -- target triple
                String -> -- target CPU
                Word -> -- Optimization degree
                FilePath -> -- output file name
                OutputType ->
                IO ()
codegenLLVM' defs triple cpu optimize file outty = do
             defTrip <- Target.getDefaultTargetTriple 
             let layout = defaultDataLayout LittleEndian
             let ast = codegen file (Target (fromSBS defTrip) layout) (map snd defs)
             outputModule triple file outty ast

failInIO :: ErrorT String IO a -> IO a
failInIO = either fail return <=< runErrorT

getCompiler = fromMaybe "clang" <$> lookupEnv "IDRIS_CLANG"

outputModule :: Machine -> FilePath -> OutputType -> Module -> IO ()
outputModule _  file Raw m = do   
    withContext $ \ctx ->
        LLVM.withModuleFromAST ctx m $ \mod ->
            LLVM.writeLLVMAssemblyToFile (LLVM.File file) mod  
outputModule _ file Object m = error "Not implemented yet"
outputModule _ file Executable m = do
    withContext $ \ctx ->
        LLVM.withModuleFromAST ctx m $ \mod ->
            withTmpFile ".ll" $ \tf -> do
                LLVM.writeLLVMAssemblyToFile (LLVM.File tf) mod
                cc <- getCompiler
                rts <- getRtsDir
                _ <- rawSystem cc ([tf, rts ++ "libidris_rts.a", "-lgc", "-lgmp", "-o" ++ file])
                return ()
  
  
withTmpFile :: String -> (FilePath -> IO a) -> IO a
withTmpFile suffix f = do
  (path, handle) <- tempfile suffix
  hClose handle
  result <- f path
  removeFile path
  return result

ierror :: String -> a
ierror msg = error $ "INTERNAL ERROR: IRTS.CodegenLLVM: " ++ msg


-- Type helpers
---------------
glRef n = C.GlobalReference (tyRef n) (mkName n)
globalRef n = ConstantOperand $ glRef n


i8 = IntegerType 8
i16 = IntegerType 16
i32 = IntegerType 32
i64 = IntegerType 64

f32 = FloatingPointType FloatFP
f64 = FloatingPointType DoubleFP

-- pointers
ptr t = PointerType t (AddrSpace 0) 
ptrI8 =  ptr i8 
ppI8 = ptr ptrI8

pmpz = PointerType mpzTy (AddrSpace 0)

-- value helpers
----------------

ci32 v = ConstantOperand (C.Int 32 v)

mainDef :: Global
mainDef =
    functionDefaults
    { G.returnType = i32
    , G.parameters =
        ([ Parameter i32 (mkName "argc") []
         , Parameter ppI8  (mkName "argv") []
         ], False)
    , G.name = mkName "main"
    , G.basicBlocks =
        [ BasicBlock (UnName 0)
          [ Do $ simpleCall "GC_init" [] -- Initialize Boehm GC
          , Do $ simpleCall "__gmp_set_memory_functions"
                     [ globalRef "__idris_gmpMalloc"
                     , globalRef "__idris_gmpRealloc"
                     , globalRef "__idris_gmpFree"
                     ]
          , Do $ Store False (globalRef "__idris_argc") 
                       (LocalReference i32 (mkName "argc")) Nothing 0 []
          , Do $ Store False (globalRef "__idris_argv") 
                       (LocalReference ppI8 (mkName "argv")) Nothing 0 []
          , UnName 1 := idrCall "{runMain_0}" [] ]
          (Do $ Ret (Just (ci32 0)) [])
        ]}

initDefs :: Target -> [Definition]
initDefs tgt =
    [ TypeDefinition (mkName "valTy")
      (Just $ StructureType False
                [ i32
                , ArrayType 0 (ptr valueType)
                ])
    , TypeDefinition (mkName "mpz")
        (Just $ StructureType False [i32, i32, ptr intPtr])
    , GlobalDefinition $ globalVariableDefaults
      { G.name = mkName "__idris_intFmtStr"
      , G.linkage = L.Internal
      , G.isConstant = True
      , G.unnamedAddr = Just G.GlobalAddr
      , G.type' = ArrayType 5 (IntegerType 8)
      , G.initializer = Just $ C.Array (IntegerType 8) (map (C.Int 8 . fromIntegral . fromEnum) "%lld" ++ [C.Int 8 0])
      }
    , rtsFun "intStr" ptrI8 [IntegerType 64]
        [ BasicBlock (UnName 0)
          [ UnName 1 := simpleCall "GC_malloc_atomic" [ConstantOperand (C.Int (tgtWordSize tgt) 21)]
          , UnName 2 := simpleCall "snprintf"
                       [ LocalReference ptrI8 (UnName 1)
                       , ConstantOperand (C.Int (tgtWordSize tgt) 21)
                       , ConstantOperand $ C.GetElementPtr True (glRef "__idris_intFmtStr") [C.Int 32 0, C.Int 32 0]
                       , LocalReference f64 (UnName 0)
                       ]
          ]
          (Do $ Ret (Just (LocalReference ptrI8 (UnName 1))) [])
        ]
    , GlobalDefinition $ globalVariableDefaults
      { G.name = mkName "__idris_floatFmtStr"
      , G.linkage = L.Internal
      , G.isConstant = True
      , G.unnamedAddr = Just G.GlobalAddr
      , G.type' = ArrayType 3 (IntegerType 8)
      , G.initializer = Just $ C.Array (IntegerType 8) (map (C.Int 8 . fromIntegral . fromEnum) "%g" ++ [C.Int 8 0])
      }
    , rtsFun "floatStr" ptrI8 [f64]
        [ BasicBlock (UnName 0)
          [ UnName 1 := simpleCall "GC_malloc_atomic" [ConstantOperand (C.Int (tgtWordSize tgt) 21)]
          , UnName 2 := simpleCall "snprintf"
                       [ LocalReference ptrI8 (UnName 1)
                       , ConstantOperand (C.Int (tgtWordSize tgt) 21)
                       , ConstantOperand $ C.GetElementPtr True (glRef "__idris_floatFmtStr") [C.Int 32 0, C.Int 32 0]
                       , LocalReference f64 (UnName 0)
                       ]
          ]
          (Do $ Ret (Just (LocalReference ptrI8 (UnName 1))) [])
        ]
    , exfun "llvm.sin.f64" (f64) [ f64 ] False
    , exfun "llvm.cos.f64" (f64) [ f64 ] False
    , exfun "llvm.pow.f64"  (f64) [ f64 ] False
    , exfun "llvm.ceil.f64" (f64) [ f64 ] False
    , exfun "llvm.floor.f64" (f64) [ f64 ] False
    , exfun "llvm.exp.f64" (f64) [ f64 ] False
    , exfun "llvm.log.f64" (f64) [ f64 ] False
    , exfun "llvm.sqrt.f64" (f64) [ f64 ] False
    , exfun "tan" (f64) [ f64 ] False
    , exfun "asin" (f64) [ f64 ] False
    , exfun "acos" (f64) [ f64 ] False
    , exfun "atan" (f64) [ f64 ] False
    , exfun "llvm.trap" VoidType [] False
    , exfun "memcpy" ptrI8 [ptrI8, ptrI8, intPtr] False
    , exfun "llvm.invariant.start" (PointerType (StructureType False []) (AddrSpace 0)) [IntegerType 64, ptrI8] False
    , exfun "snprintf" (IntegerType 32) [ptrI8, intPtr, ptrI8] True
    , exfun "strcmp" (IntegerType 32) [ptrI8, ptrI8] False
    , exfun "strlen" intPtr [ptrI8] False
    , exfun "GC_init" VoidType [] False
    , exfun "GC_malloc" ptrI8 [intPtr] False
    , exfun "GC_malloc_atomic" ptrI8 [intPtr] False
    , exfun "__gmp_set_memory_functions" VoidType
                [ PointerType (FunctionType ptrI8 [intPtr] False) (AddrSpace 0)
                , PointerType (FunctionType ptrI8 [ptrI8, intPtr, intPtr] False) (AddrSpace 0)
                , PointerType (FunctionType VoidType [ptrI8, intPtr] False) (AddrSpace 0)
                ] False
    , exfun "__gmpz_init" VoidType [pmpz] False
    , exfun "__gmpz_init_set_str" (IntegerType 32) [pmpz, ptrI8, IntegerType 32] False
    , exfun "__gmpz_get_str" ptrI8 [ptrI8, IntegerType 32, pmpz] False
    , exfun "__gmpz_get_ui" intPtr [pmpz] False
    , exfun "__gmpz_cmp" (IntegerType 32) [pmpz, pmpz] False
    , exfun "__gmpz_fdiv_q_2exp" VoidType [pmpz, pmpz, intPtr] False
    , exfun "__gmpz_mul_2exp" VoidType [pmpz, pmpz, intPtr] False
    , exfun "__gmpz_get_d" (f64) [pmpz] False
    , exfun "__gmpz_set_d" VoidType [pmpz, f64] False
    , exfun "mpz_get_ull" (IntegerType 64) [pmpz] False
    , exfun "mpz_init_set_ull" VoidType [pmpz, IntegerType 64] False
    , exfun "mpz_init_set_sll" VoidType [pmpz, IntegerType 64] False
    , exfun "__idris_strCons" ptrI8 [IntegerType 8, ptrI8] False
    , exfun "__idris_readStr" ptrI8 [ptrI8] False -- Actually pointer to FILE, but it's opaque anyway
    , exfun "__idris_readChars" ptrI8 [i32, ptrI8] False
    , exfun "__idris_writeStr" i32 [ptrI8, ptrI8] False
    , exfun "__idris_registerPtr" ptrI8 [ptrI8, i32] False
    , exfun "__idris_getRegisteredPtr" ptrI8 [ptrI8] False
    , exfun "__idris_sizeofPtr" i32 [] False
    , exfun "__idris_stdin" ptrI8 [] False  
    , exfun "__idris_stdout" ptrI8 [] False  
    , exfun "__idris_stderr" ptrI8 [] False  
    , exfun "__idris_gmpMalloc" ptrI8 [intPtr] False
    , exfun "__idris_gmpRealloc" ptrI8 [ptrI8, intPtr, intPtr] False
    , exfun "__idris_gmpFree" VoidType [ptrI8, intPtr] False
    , exfun "__idris_strRev" ptrI8 [ptrI8] False
    , exfun "strtoll" (IntegerType 64) [ptrI8, PointerType ptrI8 (AddrSpace 0), IntegerType 32] False
    , exfun "strtod" (f64) [ptrI8, PointerType ptrI8 (AddrSpace 0)] False
    , exfun "putErr" VoidType [ptrI8] False
    , exfun "printf" i32 [ptrI8] True
    , exVar "__idris_argc" (IntegerType 32)
    , exVar "__idris_argv" (PointerType ptrI8 (AddrSpace 0))
    , GlobalDefinition mainDef
    ] ++ map mpzBinFun ["add", "sub", "mul", "fdiv_q", "fdiv_r", "and", "ior", "xor"]
    where
      intPtr = IntegerType (tgtWordSize tgt)
      mpzBinFun n = exfun ("__gmpz_" ++ n) VoidType [pmpz, pmpz, pmpz] False

      rtsFun :: String -> Type -> [Type] -> [BasicBlock] -> Definition
      rtsFun name rty argtys def =
          GlobalDefinition $ functionDefaults
                               { G.linkage = L.Internal
                               , G.returnType = rty
                               , G.parameters = (flip map argtys $ \ty -> Parameter ty (UnName 0) [], False)
                               , G.name = mkName $ "__idris_" ++ name
                               , G.basicBlocks = def
                               }

      exfun :: String -> Type -> [Type] -> Bool -> Definition
      exfun name rty argtys vari =
          GlobalDefinition $ functionDefaults
                               { G.returnType = rty
                               , G.name = mkName name
                               , G.parameters = (flip map argtys $ \ty -> Parameter ty (UnName 0) [], vari)
                               }
      exVar :: String -> Type -> Definition
      exVar name ty = GlobalDefinition $ globalVariableDefaults { G.name = mkName name, G.type' = ty }


codegen :: String -> Target -> [SDecl] -> Module
codegen file tgt defs = Module {
      moduleName = "idris",
      moduleSourceFileName = fromString file,
      moduleDataLayout = Just . dataLayout $ tgt,
      moduleTargetTriple = Just (fromString $ triple tgt), 
      moduleDefinitions = initDefs tgt ++ globals ++ gendefs
    }
    where
      (gendefs, _, globals) = runRWS (mapM cgDef defs) tgt initialMGS

valueType :: Type
valueType = NamedTypeReference (mkName "valTy")

nullValue :: C.Constant
nullValue = C.Null (PointerType valueType (AddrSpace 0))

primTy :: Type -> Type
primTy inner = StructureType False [IntegerType 32, inner]

mpzTy :: Type
mpzTy = NamedTypeReference (mkName "mpz")

tyRef :: String -> Type
tyRef = NamedTypeReference . mkName

conType :: Word64 -> Type
conType nargs = StructureType False
                [ IntegerType 32
                , ArrayType nargs (PointerType valueType (AddrSpace 0))
                ]

data MGS = MGS { mgsNextGlobalName :: Word
               , mgsForeignSyms :: Map String (FType, [FType])
               }

type Modgen = RWS Target [Definition] MGS

initialMGS :: MGS
initialMGS = MGS { mgsNextGlobalName = 0
                 , mgsForeignSyms = M.empty
                 }

cgDef :: SDecl -> Modgen Definition
cgDef (SFun name argNames _ expr) = do
  nextGlobal <- gets mgsNextGlobalName
  existingForeignSyms <- gets mgsForeignSyms
  tgt <- ask
  let (_, CGS { nextGlobalName = nextGlobal', foreignSyms = foreignSyms' }, (allocas, bbs, globals)) =
          runRWS (do r <- cgExpr expr
                     case r of
                       Nothing -> terminate $ Unreachable []
                       Just r' -> terminate $ Ret (Just r') [])
                 (CGR tgt (TT.showCG name))
                 (CGS 0 nextGlobal (mkName "begin") [] (map ((\n -> Just (LocalReference (NamedTypeReference n) n)) . mkName . TT.showCG) argNames) existingForeignSyms)
      entryTerm = case bbs of
                    [] -> Do $ Ret Nothing []
                    BasicBlock n _ _:_ -> Do $ Br n []
  tell globals
  put (MGS { mgsNextGlobalName = nextGlobal', mgsForeignSyms = foreignSyms' })
  return . GlobalDefinition $ functionDefaults
             { G.linkage = L.Internal
             , G.callingConvention = CC.Fast
             , G.name = mkName (TT.showCG name)
             , G.returnType = PointerType valueType (AddrSpace 0)
             , G.parameters = (flip map argNames $ \argName ->
                                   Parameter (PointerType valueType (AddrSpace 0)) (mkName (TT.showCG argName)) []
                              , False)
             , G.basicBlocks =
                 BasicBlock (mkName "entry")
                                 (map (\(n, t) -> n := Alloca t Nothing 0 []) allocas)
                                 entryTerm
                 : bbs
             }

type CGW = ([(Name, Type)], [BasicBlock], [Definition])

type Env = [Maybe Operand]

data CGS = CGS { nextName :: Word
               , nextGlobalName :: Word
               , currentBlockName :: Name
               , instAccum :: [Named Instruction]
               , lexenv :: Env
               , foreignSyms :: Map String (FType, [FType])
               }

data CGR = CGR { target :: Target
               , funcName :: String }

type Codegen = RWS CGR CGW CGS

getFuncName :: Codegen String
getFuncName = asks funcName

getGlobalUnName :: Codegen Name
getGlobalUnName = do
  i <- gets nextGlobalName
  modify $ \s -> s { nextGlobalName = 1 + i }
  return (UnName i)

getUnName :: Codegen Name
getUnName = do
  i <- gets nextName
  modify $ \s -> s { nextName = 1 + i }
  return (UnName i)

getName :: String -> Codegen Name
getName n = do
  i <- gets nextName
  modify $ \s -> s { nextName = 1 + i }
  return (mkName $ n ++ show i)

alloca :: Name -> Type -> Codegen ()
alloca n t = tell ([(n, t)], [], [])

terminate :: Terminator -> Codegen ()
terminate term = do
  name <- gets currentBlockName
  insts <- gets instAccum
  modify $ \s -> s { instAccum = ierror "Not in a block"
                   , currentBlockName = ierror "Not in a block" }
  tell ([], [BasicBlock name insts (Do term)], [])

newBlock :: Name -> Codegen ()
newBlock name = modify $ \s -> s { instAccum = []
                                 , currentBlockName = name
                                 }

inst :: Instruction -> Codegen Operand
inst i = do
  n <- getUnName
  modify $ \s -> s { instAccum = instAccum s ++ [n := i] }
  return $ LocalReference (NamedTypeReference n) n

ninst :: String -> Instruction -> Codegen Operand
ninst name i = do
  n <- getName name
  modify $ \s -> s { instAccum = instAccum s ++ [n := i] }
  return $ LocalReference (NamedTypeReference n) n

inst' :: Instruction -> Codegen ()
inst' i = modify $ \s -> s { instAccum = instAccum s ++ [Do i] }

insts :: [Named Instruction] -> Codegen ()
insts is = modify $ \s -> s { instAccum = instAccum s ++ is }

var :: LVar -> Codegen (Maybe Operand)
var (Loc level) = (!! level) <$> gets lexenv
var (Glob n) = return $ Just (globalRef (TT.showCG n))

binds :: Env -> Codegen (Maybe Operand) -> Codegen (Maybe Operand)
binds vals cg = do
  envLen <- length <$> gets lexenv
  modify $ \s -> s { lexenv = lexenv s ++ vals }
  value <- cg
  modify $ \s -> s { lexenv = take envLen $ lexenv s }
  return value

replaceElt :: Int -> a -> [a] -> [a]
replaceElt _ val [] = error "replaceElt: Ran out of list"
replaceElt 0 val (x:xs) = val:xs
replaceElt n val (x:xs) = x : replaceElt (n-1) val xs

alloc' :: Operand -> Codegen Operand
alloc' size = inst $ simpleCall "GC_malloc" [size]

allocAtomic' :: Operand -> Codegen Operand
allocAtomic' size = inst $ simpleCall "GC_malloc_atomic" [size]

alloc :: Type -> Codegen Operand
alloc ty = do
  size <- sizeOf ty
  mem <- alloc' size
  inst $ BitCast mem (PointerType ty (AddrSpace 0)) []

allocAtomic :: Type -> Codegen Operand
allocAtomic ty = do
  size <- sizeOf ty
  mem <- allocAtomic' size
  inst $ BitCast mem (PointerType ty (AddrSpace 0)) []

sizeOf :: Type -> Codegen Operand
sizeOf ty = ConstantOperand . C.PtrToInt
            (C.GetElementPtr True (C.Null (PointerType ty (AddrSpace 0))) [C.Int 32 1])
            . IntegerType <$> getWordSize

loadInv :: Operand -> Instruction
loadInv ptr = Load False ptr Nothing 0 [("invariant.load", MetadataNode [])]

tgtWordSize :: Target -> Word32
tgtWordSize (Target { dataLayout = DataLayout { pointerLayouts = l } }) =
    fst . fromJust $ M.lookup (AddrSpace 0) l

getWordSize :: Codegen Word32
getWordSize = tgtWordSize <$> asks target

cgExpr :: SExp -> Codegen (Maybe Operand)
cgExpr (SV v) = var v
cgExpr (SApp tailcall fname args) = do
  argSlots <- mapM var args
  case sequence argSlots of
    Nothing -> return Nothing
    Just argVals -> do
      fn <- var (Glob fname)
      Just <$> inst ((idrCall (TT.showCG fname) argVals)
               { tailCallKind = if tailcall then Just Tail else Nothing })
cgExpr (SLet _ varExpr bodyExpr) = do
  val <- cgExpr varExpr
  binds [val] $ cgExpr bodyExpr
cgExpr (SUpdate (Loc level) expr) = do
  val <- cgExpr expr
  modify $ \s -> s { lexenv = replaceElt level val (lexenv s) }
  return val
cgExpr (SUpdate x expr) = cgExpr expr
cgExpr (SCon _ tag name args) = do
  argSlots <- mapM var args
  case sequence argSlots of
    Nothing -> return Nothing
    Just argVals -> do
      let ty = conType . fromIntegral . length $ argVals
      con <- alloc ty
      tagPtr <- inst $ GetElementPtr True con [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 0)] []
      inst' $ Store False tagPtr (ConstantOperand (C.Int 32 (fromIntegral tag))) Nothing 0 []
      forM_ (zip argVals [0..]) $ \(arg, i) -> do
        ptr <- inst $ GetElementPtr True con [ ConstantOperand (C.Int 32 0)
                                             , ConstantOperand (C.Int 32 1)
                                             , ConstantOperand (C.Int 32 i)] []
        inst' $ Store False ptr arg Nothing 0 []
      ptrI8 <- inst $ BitCast con (PointerType (IntegerType 8) (AddrSpace 0)) []
      inst' $ simpleCall "llvm.invariant.start" [ConstantOperand $ C.Int 64 (-1), ptrI8]
      Just <$> inst (BitCast con (PointerType valueType (AddrSpace 0)) [])
cgExpr (SCase _ inspect alts) = do
  val <- var inspect
  case val of
    Nothing -> return Nothing
    Just v -> cgCase v alts
cgExpr (SChkCase inspect alts) = do
  mval <- var inspect
  case mval of
    Nothing -> return Nothing
    Just val ->
        do endBBN <- getName "endChkCase"
           notNullBBN <- getName "notNull"
           originBlock <- gets currentBlockName
           isNull <- inst $ ICmp IPred.EQ val (ConstantOperand nullValue) []
           terminate $ CondBr isNull endBBN notNullBBN []
           newBlock notNullBBN
           ptr <- inst $ BitCast val (PointerType (IntegerType 32) (AddrSpace 0)) []
           flag <- inst $ Load False ptr Nothing 0 []
           isVal <- inst $ ICmp IPred.EQ flag (ConstantOperand (C.Int 32 (-1))) []
           conBBN <- getName "constructor"
           terminate $ CondBr isVal endBBN conBBN []
           newBlock conBBN
           result <- cgCase val alts
           caseExitBlock <- gets currentBlockName
           case result of
             Nothing -> do
               terminate $ Unreachable []
               newBlock endBBN
               return $ Just val
             Just caseVal -> do
               terminate $ Br endBBN []
               newBlock endBBN
               Just <$> inst (Phi (PointerType valueType (AddrSpace 0))
                              [(val, originBlock), (val, notNullBBN), (caseVal, caseExitBlock)] [])
cgExpr (SProj conVar idx) = do
  val <- var conVar
  case val of
    Nothing -> return Nothing
    Just v ->
        do ptr <- inst $ GetElementPtr True v
                  [ ConstantOperand (C.Int 32 0)
                  , ConstantOperand (C.Int 32 1)
                  , ConstantOperand (C.Int 32 (fromIntegral idx))
                  ] []
           Just <$> inst (Load False ptr Nothing 0 [])
cgExpr (SConst c) = Just <$> cgConst c
cgExpr (SForeign rty (FStr fname) args) = do
  func <- ensureCDecl fname (toFType rty) (map (toFType . fst) args)
  argVals <- forM args $ \(fty, v) -> do
               v' <- var v
               case v' of
                 Just val -> return $ Just (toFType fty, val)
                 Nothing -> return Nothing
  case sequence argVals of
    Nothing -> return Nothing
    Just argVals' -> do
      argUVals <- mapM (uncurry unbox) argVals'
      result <- inst Call { tailCallKind = Nothing
                          , callingConvention = CC.C
                          , returnAttributes = []
                          , function = Right func
                          , arguments = map (\v -> (v, [])) argUVals
                          , functionAttributes = []
                          , metadata = []
                          }
      Just <$> box (toFType rty) result
cgExpr (SOp fn args) = do
  argVals <- mapM var args
  case sequence argVals of
    Just ops -> Just <$> cgOp fn ops
    Nothing -> return Nothing
cgExpr SNothing = return . Just . ConstantOperand $ nullValue
cgExpr (SError msg) = do
  str <- addGlobal' (ArrayType (2 + fromIntegral (length msg)) (IntegerType 8))
         (cgConst' (TT.Str (msg ++ "\n")))
  inst' $ simpleCall "putErr" [ConstantOperand $ C.GetElementPtr True str [ C.Int 32 0
                                                                          , C.Int 32 0]]
  inst' Call { tailCallKind = Just Tail
             , callingConvention = CC.C
             , returnAttributes = []
             , function = Right $ globalRef "llvm.trap"
             , arguments = []
             , functionAttributes = [Right A.NoReturn]
             , metadata = []
             }
  return Nothing

cgCase :: Operand -> [SAlt] -> Codegen (Maybe Operand)
cgCase val alts =
    case find isConstCase alts of
      Just (SConstCase (TT.BI _) _) -> cgChainCase val defExp constAlts
      Just (SConstCase (TT.Str _) _) -> cgChainCase val defExp constAlts
      Just (SConstCase _ _) -> cgPrimCase val defExp constAlts
      Nothing -> cgConCase val defExp conAlts
    where
      defExp = getDefExp =<< find isDefCase alts
      constAlts = filter isConstCase alts
      conAlts = filter isConCase alts

      isConstCase (SConstCase {}) = True
      isConstCase _ = False

      isConCase (SConCase {}) = True
      isConCase _ = False

      isDefCase (SDefaultCase _) = True
      isDefCase _ = False

      getDefExp (SDefaultCase e) = Just e
      getDefExp _ = Nothing

cgPrimCase :: Operand -> Maybe SExp -> [SAlt] -> Codegen (Maybe Operand)
cgPrimCase caseValPtr defExp alts = do
  let caseTy = case head alts of
                 SConstCase (TT.I _)   _ -> IntegerType 32
                 SConstCase (TT.B8 _)  _ -> IntegerType 8
                 SConstCase (TT.B16 _) _ -> IntegerType 16
                 SConstCase (TT.B32 _) _ -> IntegerType 32
                 SConstCase (TT.B64 _) _ -> IntegerType 64
                 SConstCase (TT.Fl _)  _ -> f64
                 SConstCase (TT.Ch _)  _ -> IntegerType 32
  realPtr <- inst $ BitCast caseValPtr (PointerType (primTy caseTy) (AddrSpace 0)) []
  valPtr <- inst $ GetElementPtr True realPtr [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 1)] []
  caseVal <- inst $ Load False valPtr Nothing 0 []
  defBlockName <- getName "default"
  exitBlockName <- getName "caseExit"
  namedAlts <- mapM (\a -> do n <- nameAlt defBlockName a; return (n, a)) alts
  terminate $ Switch caseVal defBlockName (map (\(n, SConstCase c _) -> (cgConst' c, n)) namedAlts) []
  initEnv <- gets lexenv
  defResult <- cgDefaultAlt exitBlockName defBlockName defExp
  results <- forM namedAlts $ \(name, SConstCase _ exp) -> cgAlt initEnv exitBlockName name Nothing exp
  finishCase initEnv exitBlockName (defResult:results)

cgConCase :: Operand -> Maybe SExp -> [SAlt] -> Codegen (Maybe Operand)
cgConCase con defExp alts = do
  tagPtr <- inst $ GetElementPtr True con [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 0)] []
  tag <- inst $ Load False tagPtr Nothing 0 []
  defBlockName <- getName "default"
  exitBlockName <- getName "caseExit"
  namedAlts <- mapM (\a -> do n <- nameAlt defBlockName a; return (n, a)) alts
  terminate $ Switch tag defBlockName (map (\(n, SConCase _ tag _ _ _) ->
                                                (C.Int 32 (fromIntegral tag), n)) namedAlts) []
  initEnv <- gets lexenv
  defResult <- cgDefaultAlt exitBlockName defBlockName defExp
  results <- forM namedAlts $ \(name, SConCase _ _ _ argns exp) ->
             cgAlt initEnv exitBlockName name (Just (con, map show argns)) exp
  finishCase initEnv exitBlockName (defResult:results)

cgChainCase :: Operand -> Maybe SExp -> [SAlt] -> Codegen (Maybe Operand)
cgChainCase caseValPtr defExp alts = do
  let (caseTy, comparator) =
          case head alts of
            SConstCase (TT.BI _) _ -> (FArith (ATInt ITBig), "__gmpz_cmp")
            SConstCase (TT.Str _) _ -> (FString, "strcmp")
  caseVal <- unbox caseTy caseValPtr
  defBlockName <- getName "default"
  exitBlockName <- getName "caseExit"
  namedAlts <- mapM (\a -> do n <- nameAlt defBlockName a; return (n, a)) alts
  initEnv <- gets lexenv
  results <- forM namedAlts $ \(name, SConstCase c e) ->
             do const <- unbox caseTy =<< cgConst c
                cmp <- inst $ simpleCall comparator [const, caseVal]
                cmpResult <- inst $ ICmp IPred.EQ cmp (ConstantOperand (C.Int 32 0)) []
                elseName <- getName "else"
                terminate $ CondBr cmpResult name elseName []
                result <- cgAlt initEnv exitBlockName name Nothing e
                newBlock elseName
                return result
  modify $ \s -> s { lexenv = initEnv }
  fname <- getFuncName
  defaultVal <- cgExpr (fromMaybe (SError $ "Inexhaustive case failure in " ++ fname) defExp)
  defaultBlock <- gets currentBlockName
  defaultEnv <- gets lexenv
  defResult <- case defaultVal of
                 Just v -> do
                   terminate $ Br exitBlockName []
                   return $ Just (v, defaultBlock, defaultEnv)
                 Nothing -> do
                   terminate $ Unreachable []
                   return Nothing
  finishCase initEnv exitBlockName (defResult:results)

finishCase :: Env -> Name -> [Maybe (Operand, Name, Env)] -> Codegen (Maybe Operand)
finishCase initEnv exitBlockName results = do
  let definedResults = mapMaybe id results
  case definedResults of
    [] -> do modify $ \s -> s { lexenv = initEnv }
             return Nothing
    xs -> do
      newBlock exitBlockName
      mergeEnvs $ map (\(_, altBlock, altEnv) -> (altBlock, altEnv)) xs
      Just <$> inst (Phi (PointerType valueType (AddrSpace 0))
                (map (\(altVal, altBlock, _) -> (altVal, altBlock)) xs) [])


cgDefaultAlt :: Name -> Name -> Maybe SExp -> Codegen (Maybe (Operand, Name, Env))
cgDefaultAlt exitName name exp = do
  newBlock name
  fname <- getFuncName
  val <- cgExpr (fromMaybe (SError $ "Inexhaustive case failure in " ++ fname) exp)
  env <- gets lexenv
  block <- gets currentBlockName
  case val of
    Just v ->
        do terminate $ Br exitName []
           return $ Just (v, block, env)
    Nothing ->
        do terminate $ Unreachable []
           return Nothing

cgAlt :: Env -> Name -> Name -> Maybe (Operand, [String]) -> SExp
      -> Codegen (Maybe (Operand, Name, Env))
cgAlt initEnv exitBlockName name destr exp = do
  modify $ \s -> s { lexenv = initEnv }
  newBlock name
  locals <- case destr of
              Nothing -> return []
              Just (con, argns) ->
                  forM (zip argns [0..]) $ \(argName, i) ->
                      do ptr <- inst $ GetElementPtr True con [ ConstantOperand (C.Int 32 0)
                                                              , ConstantOperand (C.Int 32 1)
                                                              , ConstantOperand (C.Int 32 i)] []
                         Just <$> ninst argName (Load False ptr Nothing 0 [])
  altVal <- binds locals $ cgExpr exp
  altEnv <- gets lexenv
  altBlock <- gets currentBlockName
  case altVal of
    Just v -> do
      terminate $ Br exitBlockName []
      return $ Just (v, altBlock, altEnv)
    Nothing -> do
      terminate $ Unreachable []
      return Nothing

mergeEnvs :: [(Name, Env)] -> Codegen ()
mergeEnvs es = do
  let vars = transpose
             . map (\(block, env) -> map (\x -> (x, block)) env)
             $ es
  env <- forM vars $ \var ->
         case var of
           [] -> ierror "mergeEnvs: impossible"
           [(v, _)] -> return v
           vs@((v, _):_)
                  | all (== v) (map fst vs) -> return v
                  | otherwise ->
                      let valid = map (\(a, b) -> (fromJust a, b)) . filter (isJust . fst) $ vs in
                      Just <$> inst (Phi (PointerType valueType (AddrSpace 0)) valid [])
  modify $ \s -> s { lexenv = env }

nameAlt :: Name -> SAlt -> Codegen Name
nameAlt d (SDefaultCase _) = return d
nameAlt _ (SConCase _ _ name _ _) = getName (show name)
nameAlt _ (SConstCase const _) = getName (show const)

isHeapFTy :: FType -> Bool
isHeapFTy f = elem f [FString, FPtr, FAny, FArith (ATInt ITBig)]

box :: FType -> Operand -> Codegen Operand
box FUnit _ = return $ ConstantOperand nullValue
box fty fval = do
  let ty = primTy (ftyToTy fty)
  val <- if isHeapFTy fty then alloc ty else allocAtomic ty
  tagptr <- inst $ GetElementPtr True val [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 0)] []
  valptr <- inst $ GetElementPtr True val [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 1)] []
  inst' $ Store False tagptr (ci32 (-1)) Nothing 0 []
  inst' $ Store False valptr fval Nothing 0 []
  p <- inst $ BitCast val ptrI8 []
  inst' $ simpleCall "llvm.invariant.start" [ConstantOperand $ C.Int 64 (-1), p]
  inst $ BitCast val (PointerType valueType (AddrSpace 0)) []

unbox :: FType -> Operand -> Codegen Operand
unbox FUnit x = return x
unbox fty bval = do
  val <- inst $ BitCast bval (PointerType (primTy (ftyToTy fty)) (AddrSpace 0)) []
  fvalptr <- inst $ GetElementPtr True val [ci32 0, ci32 1] []
  inst $ Load False fvalptr Nothing 0 []



cgConst' :: TT.Const -> C.Constant
cgConst' (TT.I i) = C.Int 32 (fromIntegral i)
cgConst' (TT.B8  i) = C.Int 8  (fromIntegral i)
cgConst' (TT.B16 i) = C.Int 16 (fromIntegral i)
cgConst' (TT.B32 i) = C.Int 32 (fromIntegral i)
cgConst' (TT.B64 i) = C.Int 64 (fromIntegral i)

cgConst' (TT.BI i) = C.Array i8 (map (C.Int 8 . fromIntegral . fromEnum) (show i) ++ [C.Int 8 0])
cgConst' (TT.Fl f) = C.Float (F.Double f)
cgConst' (TT.Ch c) = C.Int 32 . fromIntegral . fromEnum $ c
cgConst' (TT.Str s) = C.Array i8 (map (C.Int 8 . fromIntegral . fromEnum) s ++ [C.Int 8 0])
cgConst' x = ierror $ "Unsupported constant: " ++ show x

cgConst :: TT.Const -> Codegen Operand
cgConst c@(TT.I _) = box (FArith (ATInt ITNative)) (ConstantOperand $ cgConst' c)
cgConst c@(TT.B8  _) = box (FArith (ATInt (ITFixed IT8))) (ConstantOperand $ cgConst' c)
cgConst c@(TT.B16 _) = box (FArith (ATInt (ITFixed IT16))) (ConstantOperand $ cgConst' c)
cgConst c@(TT.B32 _) = box (FArith (ATInt (ITFixed IT32))) (ConstantOperand $ cgConst' c)
cgConst c@(TT.B64 _) = box (FArith (ATInt (ITFixed IT64))) (ConstantOperand $ cgConst' c)

cgConst c@(TT.Fl _) = box (FArith ATFloat) (ConstantOperand $ cgConst' c)
cgConst c@(TT.Ch _) = box (FArith (ATInt ITChar)) (ConstantOperand $ cgConst' c)
cgConst c@(TT.Str s) = do
  str <- addGlobal' (ArrayType (1 + fromIntegral (length s)) (IntegerType 8)) (cgConst' c)
  box FString (ConstantOperand $ C.GetElementPtr True str [C.Int 32 0, C.Int 32 0])
cgConst c@(TT.BI i) = do
  let stringRepLen = (if i < 0 then 2 else 1) + fromInteger (numDigits 10 i)
  str <- addGlobal' (ArrayType stringRepLen (IntegerType 8)) (cgConst' c)
  mpz <- alloc mpzTy
  inst $ simpleCall "__gmpz_init_set_str" [mpz
                                          , ConstantOperand $ C.GetElementPtr True str [ C.Int 32 0, C.Int 32 0]
                                          , ConstantOperand $ C.Int 32 10
                                          ]
  box (FArith (ATInt ITBig)) mpz
cgConst x = return $ ConstantOperand nullValue

numDigits :: Integer -> Integer -> Integer
numDigits b n = 1 + fst (ilog b (abs n)) where
    ilog b n
        | n < b     = (0, n)
        | otherwise = let (e, r) = ilog (b*b) n
                      in  if r < b then (2*e, r) else (2*e+1, r `div` b)

addGlobal :: Global -> Codegen ()
addGlobal def = tell ([], [], [GlobalDefinition def])

addGlobal' :: Type -> C.Constant -> Codegen C.Constant
addGlobal' ty val = do
  name <- getGlobalUnName
  addGlobal $ globalVariableDefaults
                { G.name = name
                , G.linkage = L.Internal
                , G.unnamedAddr = Just G.GlobalAddr
                , G.isConstant = True
                , G.type' = ty
                , G.initializer = Just val
                }
  return $ C.GlobalReference ty name


ensureCDecl :: String -> FType -> [FType] -> Codegen Operand
ensureCDecl name rty argtys = do
  syms <- gets foreignSyms
  case M.lookup name syms of
    Nothing -> do addGlobal (ffunDecl name rty argtys)
                  modify $ \s -> s { foreignSyms = M.insert name (rty, argtys) (foreignSyms s) }
    Just (rty', argtys') -> unless (rty == rty' && argtys == argtys') . fail $
                            "Mismatched type declarations for foreign symbol \"" ++ name ++ "\": " ++ show (rty, argtys) ++ " vs " ++ show (rty', argtys')
  return $ globalRef name

ffunDecl :: String -> FType -> [FType] -> Global
ffunDecl name rty argtys =
    functionDefaults
    { G.returnType = ftyToTy rty
    , G.name = mkName name
    , G.parameters = (flip map argtys $ \fty ->
                          Parameter (ftyToTy fty) (UnName 0) []
                     , False)
    }

ftyToTy :: FType -> Type
ftyToTy (FArith (ATInt ITNative)) = i32
ftyToTy (FArith (ATInt ITBig)) = ptr mpzTy
ftyToTy (FArith (ATInt (ITFixed ty))) = IntegerType (fromIntegral $ nativeTyWidth ty)
ftyToTy (FArith (ATInt ITChar)) = i32
ftyToTy (FArith ATFloat) = f64
ftyToTy FString = ptrI8
ftyToTy FUnit = VoidType
ftyToTy FPtr = ptrI8
ftyToTy FManagedPtr = ptrI8
ftyToTy FCData = ptrI8
ftyToTy FFunction = error "hello"
ftyToTy FFunctionIO = error "hello"
ftyToTy FAny = valueType

-- Only use when known not to be ITBig
itWidth :: IntTy -> Word32
itWidth ITNative = 32
itWidth ITChar = 32
itWidth (ITFixed x) = fromIntegral $ nativeTyWidth x
itWidth x = ierror $ "itWidth: " ++ show x

itConst :: IntTy -> Integer -> C.Constant
itConst (ITFixed n) x = C.Int (fromIntegral $ nativeTyWidth n) x
itConst ITNative x = itConst (ITFixed IT32) x
itConst ITChar x = itConst (ITFixed IT32) x

cgOp :: PrimFn -> [Operand] -> Codegen Operand
cgOp (LTrunc ITBig ity) [x] = do
  nx <- unbox (FArith (ATInt ITBig)) x
  val <- inst $ simpleCall "mpz_get_ull" [nx]
  v <- case ity of
         (ITFixed IT64) -> return val
         _ -> inst $ Trunc val (ftyToTy (FArith (ATInt ity))) []
  box (FArith (ATInt ity)) v
cgOp (LZExt from ITBig) [x] = do
  nx <- unbox (FArith (ATInt from)) x
  nx' <- case from of
           (ITFixed IT64) -> return nx
           _ -> inst $ ZExt nx (IntegerType 64) []
  mpz <- alloc mpzTy
  inst' $ simpleCall "mpz_init_set_ull" [mpz, nx']
  box (FArith (ATInt ITBig)) mpz
cgOp (LSExt from ITBig) [x] = do
  nx <- unbox (FArith (ATInt from)) x
  nx' <- case from of
           (ITFixed IT64) -> return nx
           _ -> inst $ SExt nx (IntegerType 64) []
  mpz <- alloc mpzTy
  inst' $ simpleCall "mpz_init_set_sll" [mpz, nx']
  box (FArith (ATInt ITBig)) mpz

-- ITChar, ITNative, and IT32 all share representation
cgOp (LChInt ITNative) [x] = return x
cgOp (LIntCh ITNative) [x] = return x

cgOp (LSLt   (ATInt ITBig)) [x,y] = mpzCmp IPred.SLT x y
cgOp (LSLe   (ATInt ITBig)) [x,y] = mpzCmp IPred.SLE x y
cgOp (LEq    (ATInt ITBig)) [x,y] = mpzCmp IPred.EQ  x y
cgOp (LSGe   (ATInt ITBig)) [x,y] = mpzCmp IPred.SGE x y
cgOp (LSGt   (ATInt ITBig)) [x,y] = mpzCmp IPred.SGT x y
cgOp (LPlus  (ATInt ITBig)) [x,y] = mpzBin "add" x y
cgOp (LMinus (ATInt ITBig)) [x,y] = mpzBin "sub" x y
cgOp (LTimes (ATInt ITBig)) [x,y] = mpzBin "mul" x y
cgOp (LSDiv  (ATInt ITBig)) [x,y] = mpzBin "fdiv_q" x y
cgOp (LSRem  (ATInt ITBig)) [x,y] = mpzBin "fdiv_r" x y
cgOp (LAnd   ITBig) [x,y] = mpzBin "and" x y
cgOp (LOr    ITBig) [x,y] = mpzBin "ior" x y
cgOp (LXOr   ITBig) [x,y] = mpzBin "xor" x y
cgOp (LCompl ITBig) [x]   = mpzUn "com" x
cgOp (LSHL   ITBig) [x,y] = mpzBit "mul_2exp" x y
cgOp (LASHR  ITBig) [x,y] = mpzBit "fdiv_q_2exp" x y

cgOp (LTrunc ITNative (ITFixed to)) [x]
    | 32 >= nativeTyWidth to = iCoerce Trunc IT32 to x
cgOp (LZExt ITNative (ITFixed to)) [x]
    | 32 <= nativeTyWidth to = iCoerce ZExt IT32 to x
cgOp (LSExt ITNative (ITFixed to)) [x]
    | 32 <= nativeTyWidth to = iCoerce SExt IT32 to x

cgOp (LTrunc (ITFixed from) ITNative) [x]
    | nativeTyWidth from >= 32 = iCoerce Trunc from IT32 x
cgOp (LZExt (ITFixed from) ITNative) [x]
    | nativeTyWidth from <= 32 = iCoerce ZExt from IT32 x
cgOp (LSExt (ITFixed from) ITNative) [x]
    | nativeTyWidth from <= 32 = iCoerce SExt from IT32 x

cgOp (LTrunc (ITFixed from) (ITFixed to)) [x]
    | nativeTyWidth from > nativeTyWidth to = iCoerce Trunc from to x
cgOp (LZExt (ITFixed from) (ITFixed to)) [x]
    | nativeTyWidth from < nativeTyWidth to = iCoerce ZExt from to x
cgOp (LSExt (ITFixed from) (ITFixed to)) [x]
    | nativeTyWidth from < nativeTyWidth to = iCoerce SExt from to x

cgOp (LSLt   (ATInt ity)) [x,y] = iCmp ity IPred.SLT x y
cgOp (LSLe   (ATInt ity)) [x,y] = iCmp ity IPred.SLE x y
cgOp (LLt    ity)         [x,y] = iCmp ity IPred.ULT x y
cgOp (LLe    ity)         [x,y] = iCmp ity IPred.ULE x y
cgOp (LEq    (ATInt ity)) [x,y] = iCmp ity IPred.EQ  x y
cgOp (LSGe   (ATInt ity)) [x,y] = iCmp ity IPred.SGE x y
cgOp (LSGt   (ATInt ity)) [x,y] = iCmp ity IPred.SGT x y
cgOp (LGe    ity)         [x,y] = iCmp ity IPred.UGE x y
cgOp (LGt    ity)         [x,y] = iCmp ity IPred.UGT x y
cgOp (LPlus  ty@(ATInt _)) [x,y] = binary ty x y (Add False False)
cgOp (LMinus ty@(ATInt _)) [x,y] = binary ty x y (Sub False False)
cgOp (LTimes ty@(ATInt _)) [x,y] = binary ty x y (Mul False False)
cgOp (LSDiv  ty@(ATInt _)) [x,y] = binary ty x y (SDiv False)
cgOp (LSRem  ty@(ATInt _)) [x,y] = binary ty x y SRem
cgOp (LUDiv  ity)          [x,y] = binary (ATInt ity) x y (UDiv False)
cgOp (LURem  ity)          [x,y] = binary (ATInt ity) x y URem
cgOp (LAnd   ity)          [x,y] = binary (ATInt ity) x y And
cgOp (LOr    ity)          [x,y] = binary (ATInt ity) x y Or
cgOp (LXOr   ity)          [x,y] = binary (ATInt ity) x y Xor
cgOp (LCompl ity)          [x] = unary (ATInt ity) x (Xor . ConstantOperand $ itConst ity (-1))
cgOp (LSHL   ity)          [x,y] = binary (ATInt ity) x y (Shl False False)
cgOp (LLSHR  ity)          [x,y] = binary (ATInt ity) x y (LShr False)
cgOp (LASHR  ity)          [x,y] = binary (ATInt ity) x y (AShr False)

cgOp (LSLt   ATFloat) [x,y] = fCmp FPred.OLT x y
cgOp (LSLe   ATFloat) [x,y] = fCmp FPred.OLE x y
cgOp (LEq    ATFloat) [x,y] = fCmp FPred.OEQ x y
cgOp (LSGe   ATFloat) [x,y] = fCmp FPred.OGE x y
cgOp (LSGt   ATFloat) [x,y] = fCmp FPred.OGT x y
cgOp (LPlus  ATFloat) [x,y] = binary ATFloat x y (FAdd NoFastMathFlags)
cgOp (LMinus ATFloat) [x,y] = binary ATFloat x y (FSub NoFastMathFlags)
cgOp (LTimes ATFloat) [x,y] = binary ATFloat x y (FMul NoFastMathFlags) 
cgOp (LSDiv  ATFloat) [x,y] = binary ATFloat x y (FDiv NoFastMathFlags)

cgOp LFExp   [x] = nunary ATFloat "llvm.exp.f64" x 
cgOp LFLog   [x] = nunary ATFloat "llvm.log.f64" x
cgOp LFSin   [x] = nunary ATFloat "llvm.sin.f64" x
cgOp LFCos   [x] = nunary ATFloat "llvm.cos.f64" x
cgOp LFTan   [x] = nunary ATFloat "tan" x
cgOp LFASin  [x] = nunary ATFloat "asin" x
cgOp LFACos  [x] = nunary ATFloat "acos" x
cgOp LFATan  [x] = nunary ATFloat "atan" x
cgOp LFSqrt  [x] = nunary ATFloat "llvm.sqrt.f64" x
cgOp LFFloor [x] = nunary ATFloat "llvm.floor.f64" x
cgOp LFCeil  [x] = nunary ATFloat "llvm.ceil.f64" x
cgOp LFNegate [x] = do
	z <- box (FArith ATFloat) (ConstantOperand $ C.Float $ F.Double (-0.0))	
	binary ATFloat z x (FSub NoFastMathFlags)

cgOp (LIntFloat ITBig) [x] = do
  x' <- unbox (FArith (ATInt ITBig)) x
  uflt <- inst $ simpleCall "__gmpz_get_d" [ x' ]
  box (FArith ATFloat) uflt

cgOp (LIntFloat ity) [x] = do
  x' <- unbox (FArith (ATInt ity)) x
  x'' <- inst $ SIToFP x' (f64) []
  box (FArith ATFloat) x''

cgOp (LFloatInt ITBig) [x] = do
  x' <- unbox (FArith ATFloat) x
  z  <- alloc mpzTy
  inst' $ simpleCall "__gmpz_init" [z]
  inst' $ simpleCall "__gmpz_set_d" [ z, x' ]
  box (FArith (ATInt ITBig)) z

cgOp (LFloatInt ity) [x] = do
  x' <- unbox (FArith ATFloat) x
  x'' <- inst $ FPToSI x' (ftyToTy $ cmpResultTy ity) []
  box (FArith (ATInt ity)) x''

cgOp LFloatStr [x] = do
    x' <- unbox (FArith ATFloat) x
    ustr <- inst (idrCall "__idris_floatStr" [x'])
    box FString ustr 

cgOp LStrFloat [s] = do
  ns <- unbox FString s
  nx <- inst $ simpleCall "strtod"
        [ns
        , ConstantOperand $ C.Null (PointerType (PointerType (IntegerType 8) (AddrSpace 0)) (AddrSpace 0))
        ]
  box (FArith ATFloat) nx

cgOp LNoOp xs = return $ last xs


cgOp (LBitCast from to) [x] = do
  nx <- unbox (FArith from) x
  nx' <- inst $ BitCast nx (ftyToTy (FArith to)) []
  box (FArith to) nx'

cgOp LStrEq [x,y] = do
  x' <- unbox FString x
  y' <- unbox FString y
  cmp <- inst $ simpleCall "strcmp" [x', y']
  flag <- inst $ ICmp IPred.EQ cmp (ConstantOperand (C.Int 32 0)) []
  val <- inst $ ZExt flag (IntegerType 32) []
  box (FArith (ATInt (ITFixed IT32))) val

cgOp LStrLt [x,y] = do
  nx <- unbox FString x
  ny <- unbox FString y
  cmp <- inst $ simpleCall "strcmp" [nx, ny]
  flag <- inst $ ICmp IPred.ULT cmp (ConstantOperand (C.Int 32 0)) []
  val <- inst $ ZExt flag (IntegerType 32) []
  box (FArith (ATInt (ITFixed IT32))) val

cgOp (LIntStr ITBig) [x] = do
  x' <- unbox (FArith (ATInt ITBig)) x
  ustr <- inst $ simpleCall "__gmpz_get_str"
          [ ConstantOperand (C.Null (PointerType (IntegerType 8) (AddrSpace 0)))
          , ConstantOperand (C.Int 32 10)
          , x'
          ]
  box FString ustr
cgOp (LIntStr ity) [x] = do
  x' <- unbox (FArith (ATInt ity)) x
  x'' <- if itWidth ity < 64
         then inst $ SExt x' (IntegerType 64) []
         else return x'
  box FString =<< inst (idrCall "__idris_intStr" [x''])
cgOp (LStrInt ITBig) [s] = do
  ns <- unbox FString s
  mpz <- alloc mpzTy
  inst $ simpleCall "__gmpz_init_set_str" [mpz, ns, ConstantOperand $ C.Int 32 10]
  box (FArith (ATInt ITBig)) mpz
cgOp (LStrInt ity) [s] = do
  ns <- unbox FString s
  nx <- inst $ simpleCall "strtoll"
        [ns
        , ConstantOperand $ C.Null ppI8
        , ConstantOperand $ C.Int 32 10
        ]
  nx' <- case ity of
           (ITFixed IT64) -> return nx
           _ -> inst $ Trunc nx (IntegerType (itWidth ity)) []
  box (FArith (ATInt ity)) nx'

cgOp LStrConcat [x,y] = cgStrCat x y

cgOp LStrCons [c,s] = do
  nc <- unbox (FArith (ATInt ITChar)) c
  ns <- unbox FString s
  nc' <- inst $ Trunc nc (IntegerType 8) []
  r <- inst $ simpleCall "__idris_strCons" [nc', ns]
  box FString r

cgOp LStrHead [c] = do
  s <- unbox FString c
  c <- inst $ Load False s Nothing 0 []
  c' <- inst $ ZExt c (IntegerType 32) []
  box (FArith (ATInt ITChar)) c'

cgOp LStrIndex [s, i] = do
  ns <- unbox FString s
  ni <- unbox (FArith (ATInt (ITFixed IT32))) i
  p <- inst $ GetElementPtr True ns [ni] []
  c <- inst $ Load False p Nothing 0 []
  c' <- inst $ ZExt c (IntegerType 32) []
  box (FArith (ATInt ITChar)) c'

cgOp LStrTail [c] = do
  s <- unbox FString c
  c <- inst $ GetElementPtr True s [ConstantOperand $ C.Int 32 1] []
  box FString c

cgOp LStrLen [s] = do
  ns <- unbox FString s
  len <- inst $ simpleCall "strlen" [ns]
  ws <- getWordSize
  len' <- case ws of
            32 -> return len
            x | x > 32 -> inst $ Trunc len (IntegerType 32) []
              | x < 32 -> inst $ ZExt len (IntegerType 32) []
  box (FArith (ATInt (ITFixed IT32))) len'

cgOp LStrRev [s] = do
  ns <- unbox FString s
  box FString =<< inst (simpleCall "__idris_strRev" [ns])

cgOp LStrSubstr [x, y, z] = ignore

cgOp LReadStr [_] = do
  np <- inst $ simpleCall "__idris_stdin" []
  s <- inst $ simpleCall "__idris_readStr" [np]
  box FString s

cgOp LWriteStr [_,p] = do
  np <- unbox FPtr p
  s <- inst $ simpleCall "printf" [np]
  box (FArith (ATInt ITNative)) s


cgOp LSystemInfo [x] = ignore
cgOp LCrash [x] = ignore
cgOp LFork [x] = ignore
cgOp LPar [x] = ignore

-- TODO: ignored primitives, fill in

cgOp (LExternal pr) [_, x] | pr == sUN "prim__readFile" = do
    sp <- unbox FPtr x
    s <- inst $ simpleCall "__idris_readStr" [sp]
    box FString s

cgOp (LExternal pr) [_,len,x] | pr == sUN "prim__readChars" = do
    l <- unbox (FArith (ATInt ITNative)) len
    sp <- unbox FPtr x
    s <- inst $ simpleCall "__idris_readChars" [l, sp]
    box FString s

cgOp (LExternal pr) [_, x, s] | pr == sUN "prim__writeFile" = do
    f <- unbox FPtr x
    sp <- unbox FString s
    i <- inst $ simpleCall "__idris_writeStr" [f, sp]
    box (FArith (ATInt ITNative)) i

cgOp (LExternal pr) [] | pr == sUN "prim__stdin" = do 
    i <- inst $ simpleCall "__idris_stdin" []
    box FPtr i
cgOp (LExternal pr) [] | pr == sUN "prim__stdout" = do 
    i <- inst $ simpleCall "__idris_stdout" []
    box FPtr i
cgOp (LExternal pr) [] | pr == sUN "prim__stderr" = do 
    i <- inst $ simpleCall "__idris_stdout" []
    box FPtr i
    
cgOp (LExternal pr) [p] | pr == sUN "prim__asPtr" = do
    sp <- unbox FManagedPtr p
    s <- inst $ simpleCall "__idris_getRegisteredPtr" [sp]
    box FPtr s

cgOp (LExternal pr) [] | pr == sUN "prim__null" = box FPtr (ConstantOperand $ C.Null ptrI8)

cgOp (LExternal pr) [_] | pr == sUN "prim__vm" = ignore
cgOp (LExternal pr) [x, y] | pr == sUN "prim__eqPtr" = ptrEq x y
cgOp (LExternal pr) [x, y] | pr == sUN "prim__eqManagedPtr" = do
    a <- unbox FPtr x
    b <- unbox FPtr y
    c <- inst $ Load False a Nothing 0 []
    d <- inst $ Load False b Nothing 0 []
    e <- inst $ ICmp IPred.EQ c d []
    r <- inst $ SExt e i32 []
    box (FArith (ATInt ITNative)) r

cgOp (LExternal pr) [p, i] | pr == sUN "prim__registerPtr" = do
    l <- unbox (FArith (ATInt ITNative)) i
    sp <- unbox FPtr p
    s <- inst $ simpleCall "__idris_registerPtr" [sp, l]
    box FManagedPtr s


cgOp (LExternal pr) [_, p, o] | pr == sUN "prim__peek8" = peek (FArith (ATInt (ITFixed IT8))) p o
cgOp (LExternal pr) [_, p, o, x] | pr == sUN "prim__poke8" = poke (FArith (ATInt (ITFixed IT8))) p o x

cgOp (LExternal pr) [_, p, o] | pr == sUN "prim__peek16" = peek (FArith (ATInt (ITFixed IT16))) p o
cgOp (LExternal pr) [_, p, o, x] | pr == sUN "prim__poke16" = poke (FArith (ATInt (ITFixed IT16))) p o x
cgOp (LExternal pr) [_, p, o] | pr == sUN "prim__peek32" = peek (FArith (ATInt (ITFixed IT32))) p o 
cgOp (LExternal pr) [_, p, o, x] | pr == sUN "prim__poke32" = poke (FArith (ATInt (ITFixed IT32))) p o x
cgOp (LExternal pr) [_, p, o] | pr == sUN "prim__peek64" = peek (FArith (ATInt (ITFixed IT64))) p o
cgOp (LExternal pr) [_, p, o, x] | pr == sUN "prim__poke64" = poke (FArith (ATInt (ITFixed IT64))) p o x

cgOp (LExternal pr) [_, p, o] | pr == sUN "prim__peekPtr" = peek FPtr p o
cgOp (LExternal pr) [_, p, o, x] | pr == sUN "prim__pokePtr" = poke FPtr p o x

cgOp (LExternal pr) [_, p, o] | pr == sUN "prim__peekDouble" = peek (FArith ATFloat) p o
cgOp (LExternal pr) [_, p, o, x] | pr == sUN "prim__pokeDouble" = poke (FArith ATFloat) p o x

cgOp (LExternal pr) [_, p, o] | pr == sUN "prim__peekSingle" = peekSingle p o
cgOp (LExternal pr) [_, p, o, x] | pr == sUN "prim__pokeSingle" = pokeSingle p o x

cgOp (LExternal pr) [p, n] | pr == sUN "prim__ptrOffset" = do
    pt <- unbox FPtr p
    o <- unbox (FArith (ATInt ITNative)) n
    ws <- getWordSize
    offz <- inst $ ZExt o (IntegerType ws) []
    i <- inst $ PtrToInt pt (IntegerType ws) []
    offi <- inst $ Add False True i offz []
    offp <- inst $ IntToPtr offi ptrI8 []
    box FPtr offp

cgOp (LExternal pr) [] | pr == sUN "prim__sizeofPtr" = do
    i <- inst $ simpleCall "__idris_sizeofPtr" []
    box (FArith (ATInt ITNative)) i

cgOp prim args = ierror $ "Unimplemented primitive: <" ++ show prim ++ ">("
                  ++ intersperse ',' (take (length args) ['a'..]) ++ ")"


ignore = return $ ConstantOperand nullValue
iCoerce :: (Operand -> Type -> InstructionMetadata -> Instruction) -> NativeTy -> NativeTy -> Operand -> Codegen Operand
iCoerce _ from to x | from == to = return x
iCoerce operator from to x = do
  x' <- unbox (FArith (ATInt (ITFixed from))) x
  x'' <- inst $ operator x' (ftyToTy (FArith (ATInt (ITFixed to)))) []
  box (FArith (ATInt (ITFixed to))) x''

cgStrCat :: Operand -> Operand -> Codegen Operand
cgStrCat x y = do
  x' <- unbox FString x
  y' <- unbox FString y
  xlen <- inst $ simpleCall "strlen" [x']
  ylen <- inst $ simpleCall "strlen" [y']
  zlen <- inst $ Add False True xlen ylen []
  ws <- getWordSize
  total <- inst $ Add False True zlen (ConstantOperand (C.Int ws 1)) []
  mem <- allocAtomic' total
  inst $ simpleCall "memcpy" [mem, x', xlen]
  i <- inst $ PtrToInt mem (IntegerType ws) []
  offi <- inst $ Add False True i xlen []
  offp <- inst $ IntToPtr offi (PointerType (IntegerType 8) (AddrSpace 0)) []
  inst $ simpleCall "memcpy" [offp, y', ylen]
  j <- inst $ PtrToInt offp (IntegerType ws) []
  offj <- inst $ Add False True j ylen []
  end <- inst $ IntToPtr offj (PointerType (IntegerType 8) (AddrSpace 0)) []
  inst' $ Store False end (ConstantOperand (C.Int 8 0)) Nothing 0 []
  box FString mem

binary :: ArithTy -> Operand -> Operand
     -> (Operand -> Operand -> InstructionMetadata -> Instruction) -> Codegen Operand
binary ty x y instCon = do
  nx <- unbox (FArith ty) x
  ny <- unbox (FArith ty) y
  nr <- inst $ instCon nx ny []
  box (FArith ty) nr

unary :: ArithTy -> Operand 
    -> (Operand -> InstructionMetadata -> Instruction) -> Codegen Operand
unary ty x instCon = do
  nx <- unbox (FArith ty) x
  nr <- inst $ instCon nx []
  box (FArith ty) nr

nunary :: ArithTy -> String
     -> Operand -> Codegen Operand
nunary ty name x = do
  nx <- unbox (FArith ty) x
  nr <- inst $ simpleCall name [nx]
  box (FArith ty) nr

iCmp :: IntTy -> IPred.IntegerPredicate -> Operand -> Operand -> Codegen Operand
iCmp ity pred x y = do
  nx <- unbox (FArith (ATInt ity)) x
  ny <- unbox (FArith (ATInt ity)) y
  nr <- inst $ ICmp pred nx ny []
  nr' <- inst $ SExt nr (ftyToTy $ cmpResultTy ity) []
  box (cmpResultTy ity) nr'

fCmp :: FPred.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fCmp pred x y = do
  nx <- unbox (FArith ATFloat) x
  ny <- unbox (FArith ATFloat) y
  nr <- inst $ FCmp pred nx ny []
  box (FArith (ATInt (ITFixed IT32))) nr

cmpResultTy :: IntTy -> FType
-- cmpResultTy v@(ITVec _ _) = FArith (ATInt v)
cmpResultTy _ = FArith (ATInt (ITFixed IT32))

mpzBin :: String -> Operand -> Operand -> Codegen Operand
mpzBin name x y = do
  nx <- unbox (FArith (ATInt ITBig)) x
  ny <- unbox (FArith (ATInt ITBig)) y
  nz <- alloc mpzTy
  inst' $ simpleCall "__gmpz_init" [nz]
  inst' $ simpleCall ("__gmpz_" ++ name) [nz, nx, ny]
  box (FArith (ATInt ITBig)) nz

mpzBit :: String -> Operand -> Operand -> Codegen Operand
mpzBit name x y = do
  nx <- unbox (FArith (ATInt ITBig)) x
  ny <- unbox (FArith (ATInt ITBig)) y
  bitcnt <- inst $ simpleCall "__gmpz_get_ui" [ny]
  nz <- alloc mpzTy
  inst' $ simpleCall "__gmpz_init" [nz]
  inst' $ simpleCall ("__gmpz_" ++ name) [nz, nx, bitcnt]
  box (FArith (ATInt ITBig)) nz

mpzUn :: String -> Operand -> Codegen Operand
mpzUn name x = do
  nx <- unbox (FArith (ATInt ITBig)) x
  nz <- alloc mpzTy
  inst' $ simpleCall "__gmpz_init" [nz]
  inst' $ simpleCall ("__gmpz_" ++ name) [nz, nx]
  box (FArith (ATInt ITBig)) nz

mpzCmp :: IPred.IntegerPredicate -> Operand -> Operand -> Codegen Operand
mpzCmp pred x y = do
  nx <- unbox (FArith (ATInt ITBig)) x
  ny <- unbox (FArith (ATInt ITBig)) y
  cmp <- inst $ simpleCall "__gmpz_cmp" [nx, ny]
  result <- inst $ ICmp pred cmp (ConstantOperand (C.Int 32 0)) []
  i <- inst $ ZExt result (IntegerType 32) []
  box (FArith (ATInt (ITFixed IT32))) i


ptrEq x y = do
    a <- unbox FPtr x
    b <- unbox FPtr y
    e <- inst $ ICmp IPred.EQ a b []
    r <- inst $ SExt e i32 []
    box (FArith (ATInt ITNative)) r

peek t p o = do
    pt <- unbox FPtr p
    off <- unbox (FArith (ATInt ITNative)) o
    ws <- getWordSize
    offz <- inst $ ZExt off (IntegerType ws) []
    i <- inst $ PtrToInt pt (IntegerType ws) []
    offi <- inst $ Add False True i offz []
    offp <- inst $ IntToPtr offi (ptr $ ftyToTy t) []
    r <- inst $ Load False offp Nothing 0 []
    box t r

poke t p o x = do
    pt <- unbox FPtr p
    off <- unbox (FArith (ATInt ITNative)) o
    v <- unbox t x
    ws <- getWordSize
    offz <- inst $ ZExt off (IntegerType ws) []
    i <- inst $ PtrToInt pt (IntegerType ws) []
    offi <- inst $ Add False True i offz []
    offp <- inst $ IntToPtr offi (ptr $ ftyToTy t) []
    inst' $ Store False offp v Nothing 0 []
    box (FArith (ATInt ITNative)) (ci32 0)

peekSingle p o = do
    pt <- unbox FPtr p
    off <- unbox (FArith (ATInt ITNative)) o
    ws <- getWordSize
    offz <- inst $ ZExt off (IntegerType ws) []
    i <- inst $ PtrToInt pt (IntegerType ws) []
    offi <- inst $ Add False True i offz []
    offp <- inst $ IntToPtr offi (ptr f32) []
    r <- inst $ Load False offp Nothing 0 []
    d <- inst $ FPExt r f64 []
    box (FArith ATFloat) d

pokeSingle p o x = do
    pt <- unbox FPtr p
    off <- unbox (FArith (ATInt ITNative)) o
    v <- unbox (FArith ATFloat) x
    s <- inst $ FPTrunc v f32 []
    ws <- getWordSize
    offz <- inst $ ZExt off (IntegerType ws) []
    i <- inst $ PtrToInt pt (IntegerType ws) []
    offi <- inst $ Add False True i offz []
    offp <- inst $ IntToPtr offi (ptr $ f32) []
    inst' $ Store False offp s Nothing 0 []
    box (FArith (ATInt ITNative)) (ci32 0)



-- Deconstruct the Foreign type in the defunctionalised expression and build
-- a foreign type description.
toAType (FCon i) 
    | i == sUN "C_IntChar" = ATInt ITChar
    | i == sUN "C_IntNative" = ATInt ITNative
    | i == sUN "C_IntBits8" = ATInt (ITFixed IT8)
    | i == sUN "C_IntBits16" = ATInt (ITFixed IT16)
    | i == sUN "C_IntBits32" = ATInt (ITFixed IT32)
    | i == sUN "C_IntBits64" = ATInt (ITFixed IT64)
toAType t = error (show t ++ " not defined in toAType")

toFType (FCon c) 
    | c == sUN "C_Str" = FString
    | c == sUN "C_Float" = FArith ATFloat
    | c == sUN "C_Ptr" = FPtr
    | c == sUN "C_MPtr" = FManagedPtr
    | c == sUN "C_CData" = FCData
    | c == sUN "C_Unit" = FUnit
toFType (FApp c [_,ity]) 
    | c == sUN "C_IntT" = FArith (toAType ity)
    | c == sUN "C_FnT" = toFunType ity
toFType (FApp c [_]) 
    | c == sUN "C_Any" = FAny
toFType t = FAny

toFunType (FApp c [_,ity])
    | c == sUN "C_FnBase" = FFunction
    | c == sUN "C_FnIO" = FFunctionIO
toFunType (FApp c [_,_,_,ity])
    | c == sUN "C_Fn" = toFunType ity
toFunType _ = FAny

simpleCall :: String -> [Operand] -> Instruction
simpleCall name args =
    Call { tailCallKind = Nothing
         , callingConvention = CC.C
         , returnAttributes = []
         , function = Right $ globalRef name
         , arguments = map (\x -> (x, [])) args
         , functionAttributes = []
         , metadata = []
         }

idrCall :: String -> [Operand] -> Instruction
idrCall name args =
    Call { tailCallKind = Nothing
         , callingConvention = CC.Fast
         , returnAttributes = []
         , function = Right $ globalRef name
         , arguments = map (\x -> (x, [])) args
         , functionAttributes = []
         , metadata = []
         }

assert :: Operand -> String -> Codegen ()
assert condition message = do
  passed <- getName "assertPassed"
  failed <- getName "assertFailed"
  terminate $ CondBr condition passed failed []
  newBlock failed
  cgExpr (SError message)
  terminate $ Unreachable []
  newBlock passed
