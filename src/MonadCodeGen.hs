{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, StandaloneDeriving #-}
module MonadCodeGen
       ( Target
       , MonadCodeGen
       , ModuleGenT, ModuleGen
       , FunctionGenT, FunctionGen
       , genFunction, genGlobal
       , getLocalName
       , startBlock, endBlock
       , inst, inst'
       ) where

import Util

import LLVM.General.AST
import LLVM.General.AST.DataLayout

import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe
import Data.Map (Map)
import Data.Word
import qualified Data.Map as M

data Target = Target { triple :: String, dataLayout :: DataLayout }

class MonadCodeGen g where
  getMGS :: Monad m => g m ModuleGenState
  putMGS :: Monad m => ModuleGenState -> g m ()
  liftMG :: Monad m => ModuleGenT m a -> g m a
  modifyMGS :: (Monad m, Monad (g m)) => (ModuleGenState -> ModuleGenState) -> g m ()
  modifyMGS f = getMGS >>= putMGS . f
  getsMGS :: (Monad m, Monad (g m)) => (ModuleGenState -> a) -> g m a
  getsMGS f = getMGS >>= return . f

data ModuleGenState = ModuleGenState { nextGlobalName :: Word
                                     , definitions :: Map String Definition
                                     , globalNames :: Map String Word
                                     }

initialModuleGenState = ModuleGenState { nextGlobalName = 0
                                       , definitions = M.empty
                                       , globalNames = M.empty
                                       }

newtype ModuleGenT m a = MG (ReaderT Target (StateT ModuleGenState m) a)
  deriving (Functor, Applicative, Monad, MonadReader Target, MonadState ModuleGenState)

instance MonadTrans ModuleGenT where
  lift x = MG (lift (lift x))

instance MonadCodeGen ModuleGenT where
  getMGS = get
  putMGS = put
  liftMG = id

type ModuleGen = ModuleGenT Identity

data FunctionGenState = FunctionGenState { nextLocalName :: Word
                                         , currentBlockName :: Name
                                         , currentBlockInstructions :: [Named Instruction]
                                         , localNames :: Map String Word
                                         }

initialFunctionGenState = FunctionGenState { nextLocalName = 0
                                           , currentBlockName = Name "entry"
                                           , currentBlockInstructions = []
                                           , localNames = M.singleton "entry" 0
                                           }

data FunctionGenOutput = FunctionGenOutput { allocas :: [(Name, Type)], blocks :: [BasicBlock] }

instance Monoid FunctionGenOutput where
  mempty = FunctionGenOutput mempty mempty
  mappend (FunctionGenOutput a b) (FunctionGenOutput a' b') = FunctionGenOutput (a `mappend` a') (b `mappend` b')

newtype FunctionGenT m a = CG (WriterT FunctionGenOutput (StateT FunctionGenState (ModuleGenT m)) a)
  deriving (Functor, Applicative, Monad, MonadReader Target, MonadWriter FunctionGenOutput, MonadState FunctionGenState)

instance MonadTrans FunctionGenT where
  lift x = (liftMG (lift x))

instance MonadCodeGen FunctionGenT where
  getMGS = liftMG get
  putMGS = liftMG . put
  liftMG x = CG (lift (lift x))

type FunctionGen = FunctionGenT Identity

genModule :: Monad m => String -> ModuleGenT m () -> Target -> m Module
genModule name (MG mg) target = do
  ((), ModuleGenState {..}) <- runStateT (runReaderT mg target) initialModuleGenState
  return $ Module name (Just . dataLayout $ target) (Just . triple $ target) (map snd (M.toList definitions))

genFunction :: (MonadCodeGen n, Monad m, Monad (n m)) => String -> Global -> FunctionGenT m a -> n m C.Constant
genFunction desiredName opts (CG cg) = do
  ((_, FunctionGenOutput {..}), _) <- liftMG . runStateT (runWriterT cg) $ initialFunctionGenState
  name@(Name str) <- getGlobalName desiredName
  let def = GlobalDefinition (opts { G.name = name, G.basicBlocks = blocks })
      (params, varargs) = G.parameters opts
  modifyMGS $ \s -> s { definitions = M.insert str def (definitions s) }
  return (C.GlobalReference (typeOf opts) name)

genGlobal :: (MonadCodeGen n, Monad m, Monad (n m)) => String -> Global -> n m C.Constant
genGlobal desiredName opts = do
  name@(Name str) <- getGlobalName desiredName
  modifyMGS $ \s -> s { definitions = M.insert str (GlobalDefinition (opts { G.name = name })) (definitions s) }
  return (C.GlobalReference (typeOf opts) name)

getLocalName :: Monad m => String -> FunctionGenT m Name
getLocalName n = do names <- gets localNames
                    case M.lookup n names of
                      Nothing -> modify (\s -> s { localNames = M.insert n 0 names }) >> return (Name n)
                      Just i -> modify (\s -> s { localNames = M.insert n (succ i) names }) >> return (Name $ n ++ show i)

getGlobalName :: (MonadCodeGen n, Monad m, Monad (n m)) => String -> n m Name
getGlobalName n = do names <- liftMG $ gets globalNames
                     case M.lookup n names of
                       Nothing -> modifyMGS (\s -> s { globalNames = M.insert n 0 names }) >> return (Name n)
                       Just i -> modifyMGS (\s -> s { globalNames = M.insert n (succ i) names }) >> return (Name $ n ++ show i)

getLocalUnName :: Monad m => FunctionGenT m Name
getLocalUnName = do { i <- gets nextLocalName; modify (\s -> s { nextLocalName = succ i }); return (UnName i) }

getGlobalUnName :: (MonadCodeGen n, Monad m, Monad (n m)) => n m Name
getGlobalUnName = do { i <- getsMGS nextGlobalName; modifyMGS (\s -> s { nextGlobalName = succ i }); return (UnName i) }

inst :: Monad m => Instruction -> FunctionGenT m Operand
inst i = do
  n <- getLocalUnName
  modify $ \s -> s { currentBlockInstructions = currentBlockInstructions s ++ [n := i] }
  return $ LocalReference undefined n

inst' :: Monad m => String -> Instruction -> FunctionGenT m Operand
inst' n i = do
  n <- getLocalName n
  modify $ \s -> s { currentBlockInstructions = currentBlockInstructions s ++ [n := i] }
  return $ LocalReference undefined n

startBlock :: Monad m => Name -> FunctionGenT m ()
startBlock n = modify $ \s -> s { currentBlockName = n, currentBlockInstructions = [] }

endBlock :: Monad m => Terminator -> FunctionGenT m ()
endBlock t = do
  FunctionGenState {..} <- get
  tell (mempty { blocks = [BasicBlock currentBlockName currentBlockInstructions (Do t)] })
  modify $ \s -> s { currentBlockName = error "attempted to access current block name after terminate"
                   , currentBlockInstructions = error "attempted to access current block instructions after terminate"
                   }
