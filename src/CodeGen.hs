module CodeGen (generateCode) where

import Control.Monad
import Control.Applicative

import Data.Word
import Data.Maybe

import IRTS.Simplified
import IRTS.Lang (LVar(..), FType(..))

import qualified Idris.Core.TT as TT

import LLVM.General.AST
import LLVM.General.AST.Type
import LLVM.General.AST.Global
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.CallingConvention as CallingConvention

import MonadCodeGen
import Common
import Util
import Operators (compileOp)
import Constants (compileConst)

generateCode :: [(TT.Name, SDecl)] -> ModuleGen ()
generateCode defs = initialDefs >> mapM_ generateDef (map snd defs)

generateDef :: SDecl -> ModuleGen Global
generateDef (SFun name argNames _ expr) =
  runFunctionGen (show name)
                 (ptr valueTy)
                 (map ((\n -> (n, ptr valueTy, [])) . show) argNames) False
                 functionDefaults
                 (\args -> runCodeGen . withVars (map Just args) $ compile expr)

ierror :: String -> a
ierror msg = error $ "INTERNAL ERROR: CodeGen: " ++ msg

compile :: SExp -> CodeGen (Maybe Operand)
compile (SV v) = lookupVar v
compile (SApp _ name args) = sequence <$> mapM lookupVar args >>= maybe (pure Nothing) (fmap Just . call' (show name))
compile (SLet _ valueExpr bodyExpr) = do { value <- compile valueExpr; withVars [value] (compile bodyExpr) }
compile (SUpdate (Loc level) expr) = do { v <- compile expr; updateLocal level v; return v }
compile (SUpdate _ expr) = compile expr -- TODO: Check if this would be useful
compile (SCon _ tag name argVars) = do
  argVals <- sequence <$> mapM lookupVar argVars
  case argVals of
    Nothing -> return Nothing
    Just args -> do
      let ty = constructorTy . fromIntegral . length $ args
      valuePtr <- gcAllocValue ty
      tagPtr <- inst $ GetElementPtr True valuePtr [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 0)] []
      inst $ Store False tagPtr (ConstantOperand (C.Int 32 (fromIntegral tag))) Nothing 0 []
      forM_ (zip args [0..]) $ \(arg, i) -> do
        argPtr <- inst $ GetElementPtr True valuePtr (map (ConstantOperand . C.Int 32) [0, 1, i]) []
        inst $ Store False argPtr arg Nothing 0 []
      Just <$> inst (BitCast valuePtr (ptr valueTy) [])
compile (SProj valueVar fieldIndex) = do
  x <- lookupVar valueVar
  case x of
    Nothing -> pure Nothing
    Just valuePtr -> do
      argPtr <- inst $ GetElementPtr True valuePtr (map (ConstantOperand . C.Int 32) [0, 1, fromIntegral fieldIndex]) []
      Just <$> inst (Load False argPtr Nothing 0 [])
compile (SConst c) = Just <$> compileConst c
compile (SForeign _ _ _) = return Nothing
--   fn <- ensureCDecl name rty (map fst args)
--   sequence <$> mapM lookupVar (map snd args) >>= maybe (pure Nothing) (fmap Just . call fn)
compile (SOp op args) = sequence <$> mapM lookupVar args >>= maybe (pure Nothing) (fmap Just . compileOp op)
compile (SError s) =
-
ensureCDecl :: String -> FType -> [FType] -> CodeGen Global
ensureCDecl name rty args = do
  existing <- findGlobal (Name name)
  case existing of
    Nothing -> addFunction name (ftyToNativeTy rty) (map (\fty -> ("", ftyToNativeTy fty, [])) args) False functionDefaults
    Just g -> pure g
