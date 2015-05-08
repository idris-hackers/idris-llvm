module Constants (compileConst) where

import LLVM.General.AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F

import IRTS.Lang
import Idris.Core.TT (ArithTy(..), IntTy(..), Const(..), NativeTy(..))

import MonadCodeGen
import Common

cgConst' :: Const -> C.Constant
cgConst' (I i) = C.Int 32 (fromIntegral i)
cgConst' (B8  i) = C.Int 8  (fromIntegral i)
cgConst' (B16 i) = C.Int 16 (fromIntegral i)
cgConst' (B32 i) = C.Int 32 (fromIntegral i)
cgConst' (B64 i) = C.Int 64 (fromIntegral i)
cgConst' (BI i) = C.Array (IntegerType 8) (map (C.Int 8 . fromIntegral . fromEnum) (show i) ++ [C.Int 8 0])
cgConst' (Fl f) = C.Float (F.Double f)
cgConst' (Ch c) = C.Int 32 . fromIntegral . fromEnum $ c
cgConst' (Str s) = C.Array (IntegerType 8) (map (C.Int 8 . fromIntegral . fromEnum) s ++ [C.Int 8 0])
cgConst' x = ierror $ "Unsupported constant: " ++ show x

compileConst :: Const -> CodeGen Operand
compileConst c@(I _) = foreignToIdris (FArith (ATInt ITNative)) (ConstantOperand $ cgConst' c)
compileConst c@(B8  _) = foreignToIdris (FArith (ATInt (ITFixed IT8))) (ConstantOperand $ cgConst' c)
compileConst c@(B16 _) = foreignToIdris (FArith (ATInt (ITFixed IT16))) (ConstantOperand $ cgConst' c)
compileConst c@(B32 _) = foreignToIdris (FArith (ATInt (ITFixed IT32))) (ConstantOperand $ cgConst' c)
compileConst c@(B64 _) = foreignToIdris (FArith (ATInt (ITFixed IT64))) (ConstantOperand $ cgConst' c)
compileConst c@(Fl _) = foreignToIdris (FArith ATFloat) (ConstantOperand $ cgConst' c)
compileConst c@(Ch _) = foreignToIdris (FArith (ATInt ITChar)) (ConstantOperand $ cgConst' c)
compileConst c@(Str s) = do
  str <- addGlobal "" (ArrayType (1 + fromIntegral (length s)) (IntegerType 8))
  foreignToIdris FString (ConstantOperand $ C.GetElementPtr True str [C.Int 32 0, C.Int 32 0])
compileConst c@(BI i) = do
  let stringRepLen = (if i < 0 then 2 else 1) + fromInteger (numDigits 10 i)
  str <- "" addGlobal (ArrayType stringRepLen (IntegerType 8))
  mpz <- gcAllocValue bigIntTy
  call' "__gmpz_init_set_str" [ mpz
                              , ConstantOperand $ C.GetElementPtr True str [ C.Int 32 0, C.Int 32 0]
                              , ConstantOperand $ C.Int 32 10
                              ]
  foreignToIdris (FArith (ATInt ITBig)) mpz
 where -- god this is ugly
  numDigits base n = 1 + fst (ilog base n)
    where
      ilog b n
        | n < b     = (0, n)
        | otherwise = let (e, r) = ilog (b*b) n
                      in  if r < b then (2*e, r) else (2*e+1, r `div` b)

compileConst x = return $ ConstantOperand nullValue

