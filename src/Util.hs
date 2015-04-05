{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Util (typeOf) where

import LLVM.General.AST
import LLVM.General.AST.InlineAssembly
import LLVM.General.AST.Type
import LLVM.General.AST.Global
import LLVM.General.AST.Float

import qualified LLVM.General.AST.Constant as C

import Data.Word (Word32)

class Typed t where
  typeOf :: t -> Type

instance Typed Operand where
  typeOf (LocalReference ty _) = ty
  typeOf (ConstantOperand c) = typeOf c
  typeOf (MetadataStringOperand _) = MetadataType
  typeOf (MetadataNodeOperand _) = MetadataType

instance Typed Global where
  typeOf (GlobalVariable {..}) = type'
  typeOf (GlobalAlias {..}) = type'
  typeOf (Function {..}) = FunctionType returnType (map typeOf . fst $ parameters) (snd parameters)

instance Typed Parameter where
  typeOf (Parameter ty _ _) = ty

instance Typed C.Constant where
  typeOf (C.Int bits _) = IntegerType bits
  typeOf (C.Float (Half _)) = half
  typeOf (C.Float (Single _)) = float
  typeOf (C.Float (Double _)) = double
  typeOf (C.Float (Quadruple _ _)) = fp128
  typeOf (C.Float (X86_FP80 _ _)) = x86_fp80
  typeOf (C.Float (PPC_FP128 _ _)) = ppc_fp128
  typeOf (C.Null ty) = ty
  typeOf (C.Struct _ packed elts) = StructureType packed (map typeOf elts)
  typeOf (C.Array ty elts) = ArrayType (fromIntegral $ length elts) ty
  typeOf (C.Vector []) = error "typeOf on empty vector"
  typeOf (C.Vector (x:xs)) = VectorType (succ (fromIntegral $ length xs)) (typeOf x)
  typeOf (C.Undef ty) = ty
  typeOf (C.BlockAddress _ _) = ptr i8
  typeOf (C.GlobalReference ty _) = ty
  typeOf (C.Add  {..}) = typeOf operand0
  typeOf (C.FAdd {..}) = typeOf operand0
  typeOf (C.Sub  {..}) = typeOf operand0
  typeOf (C.FSub {..}) = typeOf operand0
  typeOf (C.Mul  {..}) = typeOf operand0
  typeOf (C.FMul {..}) = typeOf operand0
  typeOf (C.UDiv {..}) = typeOf operand0
  typeOf (C.SDiv {..}) = typeOf operand0
  typeOf (C.FDiv {..}) = typeOf operand0
  typeOf (C.URem {..}) = typeOf operand0
  typeOf (C.SRem {..}) = typeOf operand0
  typeOf (C.FRem {..}) = typeOf operand0
  typeOf (C.Shl  {..}) = typeOf operand0
  typeOf (C.LShr {..}) = typeOf operand0
  typeOf (C.AShr {..}) = typeOf operand0
  typeOf (C.And  {..}) = typeOf operand0
  typeOf (C.Or   {..}) = typeOf operand0
  typeOf (C.Xor  {..}) = typeOf operand0
  typeOf (C.GetElementPtr {..}) = typeOfGEP (typeOf address) (map ConstantOperand indices)
  typeOf (C.Trunc _ ty) = ty
  typeOf (C.ZExt _ ty) = ty
  typeOf (C.SExt _ ty) = ty
  typeOf (C.FPToUI _ ty) = ty
  typeOf (C.FPToSI _ ty) = ty
  typeOf (C.UIToFP _ ty) = ty
  typeOf (C.SIToFP _ ty) = ty
  typeOf (C.FPTrunc _ ty) = ty
  typeOf (C.FPExt _ ty) = ty
  typeOf (C.PtrToInt _ ty) = ty
  typeOf (C.IntToPtr _ ty) = ty
  typeOf (C.BitCast _ ty) = ty
  typeOf (C.ICmp {}) = i1
  typeOf (C.FCmp {}) = i1
  typeOf (C.Select {..}) = typeOf trueValue
  typeOf (C.ExtractElement v _) =
    case typeOf v of
      VectorType {..} -> elementType
      _ -> error $ "typeOf on ExtractElement on non-vector " ++ show v
  typeOf (C.InsertElement {..}) = typeOf vector
  typeOf (C.ShuffleVector {..}) = typeOf operand0
  typeOf (C.ExtractValue {..}) = typeOfExtractValue (typeOf aggregate) indices'
  typeOf (C.InsertValue {..}) = typeOf aggregate

instance Typed Instruction where
  typeOf (Add  {..}) = typeOf operand0
  typeOf (FAdd {..}) = typeOf operand0
  typeOf (Sub  {..}) = typeOf operand0
  typeOf (FSub {..}) = typeOf operand0
  typeOf (Mul  {..}) = typeOf operand0
  typeOf (FMul {..}) = typeOf operand0
  typeOf (UDiv {..}) = typeOf operand0
  typeOf (SDiv {..}) = typeOf operand0
  typeOf (FDiv {..}) = typeOf operand0
  typeOf (URem {..}) = typeOf operand0
  typeOf (SRem {..}) = typeOf operand0
  typeOf (FRem {..}) = typeOf operand0
  typeOf (Shl  {..}) = typeOf operand0
  typeOf (LShr {..}) = typeOf operand0
  typeOf (AShr {..}) = typeOf operand0
  typeOf (And  {..}) = typeOf operand0
  typeOf (Or   {..}) = typeOf operand0
  typeOf (Xor  {..}) = typeOf operand0
  typeOf (GetElementPtr {..}) = typeOfGEP (typeOf address) indices
  typeOf (Trunc {..}) = type'
  typeOf (ZExt {..}) = type'
  typeOf (SExt {..}) = type'
  typeOf (FPToUI {..}) = type'
  typeOf (FPToSI {..}) = type'
  typeOf (UIToFP {..}) = type'
  typeOf (SIToFP {..}) = type'
  typeOf (FPTrunc {..}) = type'
  typeOf (FPExt {..}) = type'
  typeOf (PtrToInt {..}) = type'
  typeOf (IntToPtr {..}) = type'
  typeOf (BitCast {..}) = type'
  typeOf (ICmp {}) = i1
  typeOf (FCmp {}) = i1
  typeOf (Select {..}) = typeOf trueValue
  typeOf (ExtractElement v _ _) =
    case typeOf v of
      VectorType {..} -> elementType
      _ -> error $ "typeOf on ExtractElement on non-vector " ++ show v
  typeOf (InsertElement {..}) = typeOf vector
  typeOf (ShuffleVector {..}) = typeOf operand0
  typeOf (ExtractValue {..}) = typeOfExtractValue (typeOf aggregate) indices'
  typeOf (InsertValue {..}) = typeOf aggregate
  typeOf (Fence {}) = void
  typeOf (CmpXchg {..}) = StructureType False [typeOf expected, i1]
  typeOf (AtomicRMW {..}) = typeOf value
  typeOf (Phi {..}) = type'
  typeOf (Call {function = Left (InlineAssembly {..})}) = type'
  typeOf (Call {function = Right op}) = typeOf op
  typeOf (VAArg {..}) = type'
  typeOf (LandingPad {..}) = type'

typeOfGEP :: Type -> [Operand] -> Type
typeOfGEP ty [] = ty
typeOfGEP (VectorType n ty@(PointerType {})) xs = VectorType n (typeOfGEP ty (map unvec xs))
  where
    unvec :: Operand -> Operand
    unvec (ConstantOperand (C.Vector (x:_))) = ConstantOperand x
    unvec x = x
typeOfGEP (PointerType ty addrspace) (_:xs) = PointerType (helper ty xs) addrspace
  where
    helper :: Type -> [Operand] -> Type
    helper ty [] = ty
    helper (ArrayType _ ty) (_:os) = helper ty os
    helper (VectorType _ ty) (_:os) = helper ty os
    helper (StructureType _ elts) (ConstantOperand (C.Int _ i):os) = helper (elts !! (fromIntegral i)) os
    helper (StructureType _ elts) (ConstantOperand c:_) =
      error $ "typeOf on GEP with ill-typed index into struct " ++ show c
    helper (StructureType _ elts) (i:_) =
      error $ "typeOf on GEP with non-constant index into struct " ++ show i
    helper ty _ =
      error $ "typeOf on GEP that indexes into value of non-aggregate type " ++ show ty
typeOfGEP x _ = error $ "typeOf on GEP on value of inappropriate type " ++ show x

typeOfExtractValue :: Type -> [Word32] -> Type
typeOfExtractValue ty [] = ty
typeOfExtractValue (ArrayType _ ty)   (_:xs) = typeOfExtractValue ty xs
typeOfExtractValue (StructureType _ tys) (i:xs) = typeOfExtractValue (tys !! fromIntegral i) xs
typeOfExtractValue x _ = error $ "typeOf on GEP that indexes into inappropriate type " ++ show x
