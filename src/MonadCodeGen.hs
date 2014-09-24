{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module MonadCodeGen
       ( Target(..)
       , MonadModuleGen, ModuleGen
       , getMGS, putMGS, askTarget
       , MonadFunctionGen, FunctionGen
       , getFGS, putFGS, tellFGO
      , runModuleGen, runFunctionGen, addFunction, addGlobal, addType
       , findGlobal
       , getLocalName
       , startBlock, endBlock
       , inst, inst'
       ) where

import Util

import LLVM.General.AST
import LLVM.General.AST.DataLayout

import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Map (Map)
import Data.Word
import qualified Data.Map as M

data Target = Target { triple :: String, dataLayout :: DataLayout }

class Monad m => MonadModuleGen m where
  getMGS :: m ModuleGenState
  putMGS :: ModuleGenState -> m ()
  askTarget :: m Target
  getsMGS :: (ModuleGenState -> a) -> m a
  getsMGS f = getMGS >>= return . f
  modifyMGS :: (ModuleGenState -> ModuleGenState) -> m ()
  modifyMGS f = getMGS >>= putMGS . f

class MonadModuleGen m => MonadFunctionGen m where
  getFGS :: m FunctionGenState
  putFGS :: FunctionGenState -> m ()
  tellFGO :: FunctionGenOutput -> m ()
  getsFGS :: (FunctionGenState -> a) -> m a
  getsFGS f = getFGS >>= return . f
  modifyFGS :: (FunctionGenState -> FunctionGenState) -> m ()
  modifyFGS f = getFGS >>= putFGS . f

data ModuleGenState = ModuleGenState { definitions :: Map Name Definition
                                     , globalNames :: Map String Word
                                     }

initialModuleGenState = ModuleGenState { definitions = M.empty
                                       , globalNames = M.empty
                                       }

newtype ModuleGen a = MG { unMG :: ReaderT Target (State ModuleGenState) a }
  deriving (Functor, Applicative, Monad, MonadReader Target, MonadState ModuleGenState)

instance MonadModuleGen ModuleGen where
  getMGS = get
  putMGS = put
  askTarget = ask

data FunctionGenState = FunctionGenState { currentBlockName :: Name
                                         , currentBlockInstructions :: [Named Instruction]
                                         , localNames :: Map String Word
                                         }

initialFunctionGenState = FunctionGenState { currentBlockName = error "no block"
                                           , currentBlockInstructions = []
                                           , localNames = M.singleton "entry" 0
                                           }

data FunctionGenOutput = FunctionGenOutput { allocas :: [(Name, Instruction)], blocks :: [BasicBlock] }

instance Monoid FunctionGenOutput where
  mempty = FunctionGenOutput mempty mempty
  mappend (FunctionGenOutput a b) (FunctionGenOutput a' b') = FunctionGenOutput (a `mappend` a') (b `mappend` b')

newtype FunctionGen a = FG (WriterT FunctionGenOutput (StateT FunctionGenState ModuleGen) a)
  deriving (Functor, Applicative, Monad, MonadReader Target, MonadWriter FunctionGenOutput, MonadState FunctionGenState)

liftMG :: ModuleGen a -> FunctionGen a
liftMG = FG . lift . lift

instance MonadModuleGen FunctionGen where
  getMGS = liftMG get
  putMGS = liftMG . put
  askTarget = liftMG ask

instance MonadFunctionGen FunctionGen where
  getFGS = get
  putFGS = put
  tellFGO = tell

runModuleGen :: String -> ModuleGen a -> Target -> Module
runModuleGen name (MG mg) target =
  let (_, ModuleGenState {..}) = runState (runReaderT mg target) initialModuleGenState
  in Module name (Just . dataLayout $ target) (Just . triple $ target) (map snd (M.toList definitions))

runFunctionGen :: MonadModuleGen m =>
                  String -> Type -> [(String, Type, [A.ParameterAttribute])] -> Bool -> Global ->
                  ([Operand] -> FunctionGen a) ->
                  m Global
runFunctionGen desiredName rty args vararg opts functionGen = do
  let (FG gen) = do params <- forM args $ \(n, t, a) -> do
                                            ln <- getLocalName n
                                            return $ Parameter t ln a
                    functionGen (map (\(Parameter t ln a) -> (LocalReference t ln)) params)
                    return params
  target <- askTarget
  initMGS <- getMGS
  let (((params, FunctionGenOutput {..}), _), outMGS) = runState (runReaderT (unMG $ runStateT (runWriterT gen) $ initialFunctionGenState) target) initMGS
  putMGS outMGS
  name <- getGlobalName desiredName
  let blocks' = case blocks of
                  [] -> []
                  (BasicBlock name instrs term):xs ->
                    (BasicBlock name (map (uncurry (:=)) (reverse allocas) ++ instrs) term):xs
      global = opts { G.name = name, G.basicBlocks = blocks', G.parameters = (params, vararg), G.returnType = rty }
      (params, varargs) = G.parameters opts
  modifyMGS $ \s -> s { definitions = M.insert name (GlobalDefinition global) (definitions s) }
  return global

addFunction :: MonadModuleGen m => String -> Type -> [(String, Type, [A.ParameterAttribute])] -> Bool -> Global -> m Global
addFunction n r a v o = runFunctionGen n r a v o (const (return ()))

addType :: MonadModuleGen m => String -> Type -> m Type
addType desiredName ty = do
  name <- getGlobalName desiredName
  modifyMGS $ \s -> s { definitions = M.insert name (TypeDefinition name (Just ty)) (definitions s) }
  return (NamedTypeReference name)

addGlobal :: MonadModuleGen m => String -> Global -> m C.Constant
addGlobal desiredName opts = do
  name <- getGlobalName desiredName
  modifyMGS $ \s -> s { definitions = M.insert name (GlobalDefinition (opts { G.name = name })) (definitions s) }
  return (C.GlobalReference (typeOf opts) name)

findGlobal :: MonadModuleGen m => Name -> m (Maybe Global)
findGlobal name = getsMGS $ \s -> case (M.lookup name (definitions s)) of
                                    Just (GlobalDefinition g) -> Just g
                                    _ -> Nothing

smartName :: String -> Word -> Name
smartName "" i = UnName i
smartName n i = Name (n ++ show i)

getLocalName :: MonadFunctionGen m => String -> m Name
getLocalName n = do names <- getsFGS localNames
                    case M.lookup n names of
                      Nothing -> modifyFGS (\s -> s { localNames = M.insert n 0 names }) >> return (smartName n 0)
                      Just i -> modifyFGS (\s -> s { localNames = M.insert n (succ i) names }) >> return (smartName n i)

getGlobalName :: MonadModuleGen m => String -> m Name
getGlobalName n = do names <- getsMGS globalNames
                     case M.lookup n names of
                       Nothing -> modifyMGS (\s -> s { globalNames = M.insert n 0 names }) >> return (smartName n 0)
                       Just i -> modifyMGS (\s -> s { globalNames = M.insert n (succ i) names }) >> return (smartName n i)

inst :: MonadFunctionGen m => Instruction -> m Operand
inst = inst' ""

inst' :: MonadFunctionGen m => String -> Instruction -> m Operand
inst' n i = do
  n <- getLocalName n
  modifyFGS $ \s -> s { currentBlockInstructions = currentBlockInstructions s ++ [n := i] }
  return $ LocalReference undefined n

startBlock :: MonadFunctionGen m => Name -> m ()
startBlock n = modifyFGS $ \s -> s { currentBlockName = n, currentBlockInstructions = [] }

endBlock :: MonadFunctionGen m => Terminator -> m ()
endBlock t = do
  FunctionGenState {..} <- getFGS
  tellFGO (mempty { blocks = [BasicBlock currentBlockName currentBlockInstructions (Do t)] })
  modifyFGS $ \s -> s { currentBlockName = error "attempted to access current block name after terminate"
                      , currentBlockInstructions = error "attempted to access current block instructions after terminate"
                      }
