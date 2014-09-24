module Constants (compileConst) where

import LLVM.General.AST
import qualified LLVM.General.AST.Constant as C
import Idris.Core.TT (Const(..))

import MonadCodeGen
import Common

compileConst :: Const -> CodeGen Operand
compileConst = undefined
