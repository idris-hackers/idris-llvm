{-# LANGUAGE RecordWildCards #-}
module Operators (compileOp) where

import Control.Monad

import Data.Word
import Data.List

import LLVM.General.AST
import LLVM.General.AST.Type
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.IntegerPredicate as IPred
import qualified LLVM.General.AST.FloatingPointPredicate as FPred

import IRTS.Lang (PrimFn(..), FType(..))
import Idris.Core.TT (ArithTy(..), IntTy(..), NativeTy(..), nativeTyWidth)

import MonadCodeGen
import Common

compileOp :: PrimFn -> [Operand] -> CodeGen Operand
compileOp = undefined
-- compileOp (LTrunc ITBig ity) [x] = do
--   nx <- unbox (FArith (ATInt ITBig)) x
--   val <- call' "mpz_get_ull" [nx]
--   v <- case ity of
--          (ITFixed IT64) -> return val
--          _ -> inst $ Trunc val (ftyToNativeTy (FArith (ATInt ity))) []
--   box (FArith (ATInt ity)) v
-- compileOp (LZExt from ITBig) [x] = do
--   nx <- unbox (FArith (ATInt from)) x
--   nx' <- case from of
--            (ITFixed IT64) -> return nx
--            _ -> inst $ ZExt nx i64 []
--   mpz <- gcAllocValue bigIntTy
--   inst' $ call' "mpz_init_set_ull" [mpz, nx']
--   box (FArith (ATInt ITBig)) mpz
-- compileOp (LSExt from ITBig) [x] = do
--   nx <- unbox (FArith (ATInt from)) x
--   nx' <- case from of
--            (ITFixed IT64) -> return nx
--            _ -> inst $ SExt nx i64 []
--   mpz <- gcAllocValue bigIntTy
--   inst' $ call' "mpz_init_set_sll" [mpz, nx']
--   box (FArith (ATInt ITBig)) mpz

-- -- ITChar, ITNative, and IT32 all share representation
-- compileOp (LChInt ITNative) [x] = return x
-- compileOp (LIntCh ITNative) [x] = return x

-- compileOp (LSLt   (ATInt ITBig)) [x,y] = mpzCmp IPred.SLT x y
-- compileOp (LSLe   (ATInt ITBig)) [x,y] = mpzCmp IPred.SLE x y
-- compileOp (LEq    (ATInt ITBig)) [x,y] = mpzCmp IPred.EQ  x y
-- compileOp (LSGe   (ATInt ITBig)) [x,y] = mpzCmp IPred.SGE x y
-- compileOp (LSGt   (ATInt ITBig)) [x,y] = mpzCmp IPred.SGT x y
-- compileOp (LPlus  (ATInt ITBig)) [x,y] = mpzBin "add" x y
-- compileOp (LMinus (ATInt ITBig)) [x,y] = mpzBin "sub" x y
-- compileOp (LTimes (ATInt ITBig)) [x,y] = mpzBin "mul" x y
-- compileOp (LSDiv  (ATInt ITBig)) [x,y] = mpzBin "fdiv_q" x y
-- compileOp (LSRem  (ATInt ITBig)) [x,y] = mpzBin "fdiv_r" x y
-- compileOp (LAnd   ITBig) [x,y] = mpzBin "and" x y
-- compileOp (LOr    ITBig) [x,y] = mpzBin "ior" x y
-- compileOp (LXOr   ITBig) [x,y] = mpzBin "xor" x y
-- compileOp (LCompl ITBig) [x]   = mpzUn "com" x
-- compileOp (LSHL   ITBig) [x,y] = mpzBit "mul_2exp" x y
-- compileOp (LASHR  ITBig) [x,y] = mpzBit "fdiv_q_2exp" x y

-- compileOp (LTrunc ITNative (ITFixed to)) [x]
--     | 32 >= nativeTyWidth to = iCoerce Trunc IT32 to x
-- compileOp (LZExt ITNative (ITFixed to)) [x]
--     | 32 <= nativeTyWidth to = iCoerce ZExt IT32 to x
-- compileOp (LSExt ITNative (ITFixed to)) [x]
--     | 32 <= nativeTyWidth to = iCoerce SExt IT32 to x

-- compileOp (LTrunc (ITFixed from) ITNative) [x]
--     | nativeTyWidth from >= 32 = iCoerce Trunc from IT32 x
-- compileOp (LZExt (ITFixed from) ITNative) [x]
--     | nativeTyWidth from <= 32 = iCoerce ZExt from IT32 x
-- compileOp (LSExt (ITFixed from) ITNative) [x]
--     | nativeTyWidth from <= 32 = iCoerce SExt from IT32 x

-- compileOp (LTrunc (ITFixed from) (ITFixed to)) [x]
--     | nativeTyWidth from > nativeTyWidth to = iCoerce Trunc from to x
-- compileOp (LZExt (ITFixed from) (ITFixed to)) [x]
--     | nativeTyWidth from < nativeTyWidth to = iCoerce ZExt from to x
-- compileOp (LSExt (ITFixed from) (ITFixed to)) [x]
--     | nativeTyWidth from < nativeTyWidth to = iCoerce SExt from to x

-- compileOp (LSLt   (ATInt ity)) [x,y] = iCmp ity IPred.SLT x y
-- compileOp (LSLe   (ATInt ity)) [x,y] = iCmp ity IPred.SLE x y
-- compileOp (LLt    ity)         [x,y] = iCmp ity IPred.ULT x y
-- compileOp (LLe    ity)         [x,y] = iCmp ity IPred.ULE x y
-- compileOp (LEq    (ATInt ity)) [x,y] = iCmp ity IPred.EQ  x y
-- compileOp (LSGe   (ATInt ity)) [x,y] = iCmp ity IPred.SGE x y
-- compileOp (LSGt   (ATInt ity)) [x,y] = iCmp ity IPred.SGT x y
-- compileOp (LGe    ity)         [x,y] = iCmp ity IPred.UGE x y
-- compileOp (LGt    ity)         [x,y] = iCmp ity IPred.UGT x y
-- compileOp (LPlus  ty@(ATInt _)) [x,y] = binary ty x y (Add False False)
-- compileOp (LMinus ty@(ATInt _)) [x,y] = binary ty x y (Sub False False)
-- compileOp (LTimes ty@(ATInt _)) [x,y] = binary ty x y (Mul False False)
-- compileOp (LSDiv  ty@(ATInt _)) [x,y] = binary ty x y (SDiv False)
-- compileOp (LSRem  ty@(ATInt _)) [x,y] = binary ty x y SRem
-- compileOp (LUDiv  ity)          [x,y] = binary (ATInt ity) x y (UDiv False)
-- compileOp (LURem  ity)          [x,y] = binary (ATInt ity) x y URem
-- compileOp (LAnd   ity)          [x,y] = binary (ATInt ity) x y And
-- compileOp (LOr    ity)          [x,y] = binary (ATInt ity) x y Or
-- compileOp (LXOr   ity)          [x,y] = binary (ATInt ity) x y Xor
-- compileOp (LCompl ity)          [x] = unary (ATInt ity) x (Xor . ConstantOperand $ itConst ity (-1))
-- compileOp (LSHL   ity)          [x,y] = binary (ATInt ity) x y (Shl False False)
-- compileOp (LLSHR  ity)          [x,y] = binary (ATInt ity) x y (LShr False)
-- compileOp (LASHR  ity)          [x,y] = binary (ATInt ity) x y (AShr False)

-- compileOp (LSLt   ATFloat) [x,y] = fCmp FPred.OLT x y
-- compileOp (LSLe   ATFloat) [x,y] = fCmp FPred.OLE x y
-- compileOp (LEq    ATFloat) [x,y] = fCmp FPred.OEQ x y
-- compileOp (LSGe   ATFloat) [x,y] = fCmp FPred.OGE x y
-- compileOp (LSGt   ATFloat) [x,y] = fCmp FPred.OGT x y
-- compileOp (LPlus  ATFloat) [x,y] = binary ATFloat x y FAdd
-- compileOp (LMinus ATFloat) [x,y] = binary ATFloat x y FSub
-- compileOp (LTimes ATFloat) [x,y] = binary ATFloat x y FMul 
-- compileOp (LSDiv  ATFloat) [x,y] = binary ATFloat x y FDiv

-- compileOp LFExp   [x] = nunary ATFloat "llvm.exp.f64" x 
-- compileOp LFLog   [x] = nunary ATFloat "llvm.log.f64" x
-- compileOp LFSin   [x] = nunary ATFloat "llvm.sin.f64" x
-- compileOp LFCos   [x] = nunary ATFloat "llvm.cos.f64" x
-- compileOp LFTan   [x] = nunary ATFloat "tan" x
-- compileOp LFASin  [x] = nunary ATFloat "asin" x
-- compileOp LFACos  [x] = nunary ATFloat "acos" x
-- compileOp LFATan  [x] = nunary ATFloat "atan" x
-- compileOp LFSqrt  [x] = nunary ATFloat "llvm.sqrt.f64" x
-- compileOp LFFloor [x] = nunary ATFloat "llvm.floor.f64" x
-- compileOp LFCeil  [x] = nunary ATFloat "llvm.ceil.f64" x

-- compileOp (LIntFloat ITBig) [x] = do
--   x' <- unbox (FArith (ATInt ITBig)) x
--   uflt <- call' "__gmpz_get_d" [ x' ]
--   box (FArith ATFloat) uflt

-- compileOp (LIntFloat ity) [x] = do
--   x' <- unbox (FArith (ATInt ity)) x
--   x'' <- inst $ SIToFP x' (FloatingPointType 64 IEEE) []
--   box (FArith ATFloat) x''

-- compileOp (LFloatInt ITBig) [x] = do
--   x' <- unbox (FArith ATFloat) x
--   z  <- gcAllocValue bigIntTy
--   inst' $ call' "__gmpz_init" [z]
--   inst' $ call' "__gmpz_set_d" [ z, x' ]
--   box (FArith (ATInt ITBig)) z

-- compileOp (LFloatInt ity) [x] = do
--   x' <- unbox (FArith ATFloat) x
--   x'' <- inst $ FPToSI x' (ftyToNativeTy $ cmpResultTy ity) []
--   box (FArith (ATInt ity)) x''

-- compileOp LFloatStr [x] = do
--     x' <- unbox (FArith ATFloat) x
--     ustr <- inst (call "__idris_floatStr" [x']) -- TODO: Generate the code here directly
--     box FString ustr

-- compileOp LNoOp xs = return $ last xs

-- compileOp (LMkVec ety c) xs | c == length xs = do
--   nxs <- mapM (unbox (FArith (ATInt (ITFixed ety)))) xs
--   vec <- foldM (\v (e, i) -> inst $ InsertElement v e (ConstantOperand (C.Int 32 i)) [])
--                (ConstantOperand $ C.Vector (replicate c (C.Undef (IntegerType . fromIntegral $ nativeTyWidth ety))))
--                (zip nxs [0..])
--   box (FArith (ATInt (ITVec ety c))) vec

-- compileOp (LIdxVec ety c) [v,i] = do
--   nv <- unbox (FArith (ATInt (ITVec ety c))) v
--   ni <- unbox (FArith (ATInt (ITFixed IT32))) i
--   elt <- inst $ ExtractElement nv ni []
--   box (FArith (ATInt (ITFixed ety))) elt

-- compileOp (LUpdateVec ety c) [v,i,e] = do
--   let fty = FArith (ATInt (ITVec ety c))
--   nv <- unbox fty v
--   ni <- unbox (FArith (ATInt (ITFixed IT32))) i
--   ne <- unbox (FArith (ATInt (ITFixed ety))) e
--   nv' <- inst $ InsertElement nv ne ni []
--   box fty nv'

-- compileOp (LBitCast from to) [x] = do
--   nx <- unbox (FArith from) x
--   nx' <- inst $ BitCast nx (ftyToNativeTy (FArith to)) []
--   box (FArith to) nx'

-- compileOp LStrEq [x,y] = do
--   x' <- unbox FString x
--   y' <- unbox FString y
--   cmp <- call' "strcmp" [x', y']
--   flag <- inst $ ICmp IPred.EQ cmp (ConstantOperand (C.Int 32 0)) []
--   val <- inst $ ZExt flag i32 []
--   box (FArith (ATInt (ITFixed IT32))) val

-- compileOp LStrLt [x,y] = do
--   nx <- unbox FString x
--   ny <- unbox FString y
--   cmp <- call' "strcmp" [nx, ny]
--   flag <- inst $ ICmp IPred.ULT cmp (ConstantOperand (C.Int 32 0)) []
--   val <- inst $ ZExt flag i32 []
--   box (FArith (ATInt (ITFixed IT32))) val

-- compileOp (LIntStr ITBig) [x] = do
--   x' <- unbox (FArith (ATInt ITBig)) x
--   ustr <- call' "__gmpz_get_str"
--           [ ConstantOperand (C.Null (ptr i8))
--           , ConstantOperand (C.Int 32 10)
--           , x'
--           ]
--   box FString ustr
-- compileOp (LIntStr ity) [x] = do
--   x' <- unbox (FArith (ATInt ity)) x
--   x'' <- if itWidth ity < 64
--          then inst $ SExt x' i64 []
--          else return x'
--   box FString =<< inst (call' "__idris_intStr" [x'']) -- TODO: Generate the code here directly
-- compileOp (LStrInt ITBig) [s] = do
--   ns <- unbox FString s
--   mpz <- gcAllocValue bigIntTy
--   call' "__gmpz_init_set_str" [mpz, ns, ConstantOperand $ C.Int 32 10]
--   box (FArith (ATInt ITBig)) mpz
-- compileOp (LStrInt ity) [s] = do
--   ns <- unbox FString s
--   nx <- call' "strtoll"
--         [ns
--         , ConstantOperand $ C.Null (ptr (ptr i8))
--         , ConstantOperand $ C.Int 32 10
--         ]
--   nx' <- case ity of
--            (ITFixed IT64) -> return nx
--            _ -> inst $ Trunc nx (IntegerType (itWidth ity)) []
--   box (FArith (ATInt ity)) nx'

-- compileOp LStrConcat [x,y] = cgStrCat x y

-- compileOp LStrCons [c,s] = do
--   nc <- unbox (FArith (ATInt ITChar)) c
--   ns <- unbox FString s
--   nc' <- inst $ Trunc nc i8 []
--   r <- call' "__idris_strCons" [nc', ns]
--   box FString r

-- compileOp LStrHead [c] = do
--   s <- unbox FString c
--   c <- inst $ Load False s Nothing 0 []
--   c' <- inst $ ZExt c i32 []
--   box (FArith (ATInt ITChar)) c'

-- compileOp LStrIndex [s, i] = do
--   ns <- unbox FString s
--   ni <- unbox (FArith (ATInt (ITFixed IT32))) i
--   p <- inst $ GetElementPtr True ns [ni] []
--   c <- inst $ Load False p Nothing 0 []
--   c' <- inst $ ZExt c i32 []
--   box (FArith (ATInt ITChar)) c'

-- compileOp LStrTail [c] = do
--   s <- unbox FString c
--   c <- inst $ GetElementPtr True s [ConstantOperand $ C.Int 32 1] []
--   box FString c

-- compileOp LStrLen [s] = do
--   ns <- unbox FString s
--   len <- call' "strlen" [ns]
--   ws <- getWordSize
--   len' <- case ws of
--             32 -> return len
--             x | x > 32 -> inst $ Trunc len i32 []
--               | x < 32 -> inst $ ZExt len i32 []
--   box (FArith (ATInt (ITFixed IT32))) len'

-- compileOp LStrRev [s] = do
--   ns <- unbox FString s
--   box FString =<< inst (call' "__idris_strRev" [ns])

-- compileOp LReadStr [p] = do
--   np <- unbox FPtr p
--   s <- call' "__idris_readStr" [np]
--   box FString s

-- compileOp LStdIn  [] = do
--   stdin <- getStdIn
--   ptr <- inst $ loadInv stdin
--   box FPtr ptr
-- compileOp LStdOut  [] = do
--   stdout <- getStdOut
--   ptr <- inst $ loadInv stdout
--   box FPtr ptr
-- compileOp LStdErr  [] = do
--   stdErr <- getStdErr
--   ptr <- inst $ loadInv stdErr
--   box FPtr ptr

-- compileOp LNullPtr [] = box FPtr (ConstantOperand $ C.Null (ptr i8))

-- compileOp prim args = error $ "Unimplemented primitive: <" ++ show prim ++ ">("
--                   ++ intersperse ',' (take (length args) ['a'..]) ++ ")"

-- loadInv :: Operand -> Instruction
-- loadInv ptr = Load False ptr Nothing 0 [("invariant.load", MetadataNode [])]

-- iCoerce :: (Operand -> Type -> InstructionMetadata -> Instruction) -> NativeTy -> NativeTy -> Operand -> CodeGen Operand
-- iCoerce _ from to x | from == to = return x
-- iCoerce operator from to x = do
--   x' <- unbox (FArith (ATInt (ITFixed from))) x
--   x'' <- inst $ operator x' (ftyToNativeTy (FArith (ATInt (ITFixed to)))) []
--   box (FArith (ATInt (ITFixed to))) x''

-- cgStrCat :: Operand -> Operand -> CodeGen Operand
-- cgStrCat x y = do
--   x' <- unbox FString x
--   y' <- unbox FString y
--   xlen <- call' "strlen" [x']
--   ylen <- call' "strlen" [y']
--   zlen <- inst $ Add False True xlen ylen []
--   ws <- getWordSize
--   total <- inst $ Add False True zlen (ConstantOperand (C.Int ws 1)) []
--   mem <- gcAllocValue total -- TODO: Atomic alloc
--   call' "memcpy" [mem, x', xlen]
--   i <- inst $ PtrToInt mem (IntegerType ws) []
--   offi <- inst $ Add False True i xlen []
--   offp <- inst $ IntToPtr offi (ptr i8) []
--   call' "memcpy" [offp, y', ylen]
--   j <- inst $ PtrToInt offp (IntegerType ws) []
--   offj <- inst $ Add False True j ylen []
--   end <- inst $ IntToPtr offj (ptr i8) []
--   inst' $ Store False end (ConstantOperand (C.Int 8 0)) Nothing 0 []
--   box FString mem

-- binary :: ArithTy -> Operand -> Operand
--      -> (Operand -> Operand -> InstructionMetadata -> Instruction) -> CodeGen Operand
-- binary ty x y instCon = do
--   nx <- unbox (FArith ty) x
--   ny <- unbox (FArith ty) y
--   nr <- inst $ instCon nx ny []
--   box (FArith ty) nr

-- unary :: ArithTy -> Operand 
--     -> (Operand -> InstructionMetadata -> Instruction) -> CodeGen Operand
-- unary ty x instCon = do
--   nx <- unbox (FArith ty) x
--   nr <- inst $ instCon nx []
--   box (FArith ty) nr

-- nunary :: ArithTy -> String
--      -> Operand -> CodeGen Operand
-- nunary ty name x = do
--   nx <- unbox (FArith ty) x
--   nr <- call' name [nx]
--   box (FArith ty) nr

-- iCmp :: IntTy -> IPred.IntegerPredicate -> Operand -> Operand -> CodeGen Operand
-- iCmp ity pred x y = do
--   nx <- unbox (FArith (ATInt ity)) x
--   ny <- unbox (FArith (ATInt ity)) y
--   nr <- inst $ ICmp pred nx ny []
--   nr' <- inst $ SExt nr (ftyToNativeTy $ cmpResultTy ity) []
--   box (cmpResultTy ity) nr'

-- fCmp :: FPred.FloatingPointPredicate -> Operand -> Operand -> CodeGen Operand
-- fCmp pred x y = do
--   nx <- unbox (FArith ATFloat) x
--   ny <- unbox (FArith ATFloat) y
--   nr <- inst $ FCmp pred nx ny []
--   box (FArith (ATInt (ITFixed IT32))) nr

-- cmpResultTy :: IntTy -> FType
-- cmpResultTy v@(ITVec _ _) = FArith (ATInt v)
-- cmpResultTy _ = FArith (ATInt (ITFixed IT32))

-- mpzBin :: String -> Operand -> Operand -> CodeGen Operand
-- mpzBin name x y = do
--   nx <- unbox (FArith (ATInt ITBig)) x
--   ny <- unbox (FArith (ATInt ITBig)) y
--   nz <- gcAllocValue bigIntTy
--   inst' $ call' "__gmpz_init" [nz]
--   inst' $ call' ("__gmpz_" ++ name) [nz, nx, ny]
--   box (FArith (ATInt ITBig)) nz

-- mpzBit :: String -> Operand -> Operand -> CodeGen Operand
-- mpzBit name x y = do
--   nx <- unbox (FArith (ATInt ITBig)) x
--   ny <- unbox (FArith (ATInt ITBig)) y
--   bitcnt <- call' "__gmpz_get_ui" [ny]
--   nz <- gcAllocValue bigIntTy
--   inst' $ call' "__gmpz_init" [nz]
--   inst' $ call' ("__gmpz_" ++ name) [nz, nx, bitcnt]
--   box (FArith (ATInt ITBig)) nz

-- mpzUn :: String -> Operand -> CodeGen Operand
-- mpzUn name x = do
--   nx <- unbox (FArith (ATInt ITBig)) x
--   nz <- gcAllocValue bigIntTy
--   inst' $ call' "__gmpz_init" [nz]
--   inst' $ call' ("__gmpz_" ++ name) [nz, nx]
--   box (FArith (ATInt ITBig)) nz

-- mpzCmp :: IPred.IntegerPredicate -> Operand -> Operand -> CodeGen Operand
-- mpzCmp pred x y = do
--   nx <- unbox (FArith (ATInt ITBig)) x
--   ny <- unbox (FArith (ATInt ITBig)) y
--   cmp <- call' "__gmpz_cmp" [nx, ny]
--   result <- inst $ ICmp pred cmp (ConstantOperand (C.Int 32 0)) []
--   i <- inst $ ZExt result i32 []
--   box (FArith (ATInt (ITFixed IT32))) i

-- -- Only use when known not to be ITBig
-- itWidth :: IntTy -> Word32
-- itWidth ITNative = 32
-- itWidth ITChar = 32
-- itWidth (ITFixed x) = fromIntegral $ nativeTyWidth x
-- itWidth x = error $ "itWidth: " ++ show x
