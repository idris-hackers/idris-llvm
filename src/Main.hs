{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Except

import qualified Data.Set as S

import System.Environment
import System.Exit
import System.FilePath ((</>))
import System.Directory (removeFile)
import System.IO (hClose)
import System.Process (rawSystem)

import Idris.AbsSyntax (Idris, runIO, Codegen(Via))
import Idris.ElabDecls
import Idris.REPL

import Paths_idris_llvm

import IRTS.Compiler
import IRTS.CodegenCommon
import IRTS.System (getCC)

import Util.System

import LLVM.General.Context
import LLVM.General.Target

import qualified LLVM.General.Relocation as R
import qualified LLVM.General.CodeModel as CM
import qualified LLVM.General.CodeGenOpt as CGO
import qualified LLVM.General.PassManager as PM
import qualified LLVM.General.Module as MO

import CodeGen (generateCode)
import MonadCodeGen (runModuleGen, Target(..))

data Opts = Opts { inputs :: [FilePath],
                   output :: FilePath,
                   oTargetTriple :: String,
                   oTargetCPU :: String }

showUsage :: IO ()
showUsage = do putStrLn "Usage: idris-llvm <ibc-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             triple <- getDefaultTargetTriple
             cpu <- getHostCPUName
             return $ helper (Opts [] "a.out" triple cpu) xs
  where
    helper opts ("-o":o:xs) = helper (opts { output = o }) xs
    helper opts (x:xs) = helper (opts { inputs = x:inputs opts }) xs
    helper opts [] = opts

llvm_main :: Opts -> Idris ()
llvm_main opts = do elabPrims
                    loadInputs (inputs opts) Nothing
                    mainProg <- elabMain
                    ir <- compile (Via "llvm") (output opts) (Just mainProg)
                    runIO $ compileAndOutputModule ir

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts))
             then showUsage
             else runMain (llvm_main opts)

compileAndOutputModule :: CodeGenerator
compileAndOutputModule (CodegenInfo {..}) =
  withContext $ \context -> do
  initializeAllTargets
  (target, _) <- failInIO $ lookupTarget Nothing targetTriple
  withTargetOptions $ \options -> do
  withTargetMachine target targetTriple targetCPU S.empty options R.Default CM.Default CGO.Default $ \targetMachine -> do
  layout <- getTargetMachineDataLayout targetMachine
  failInIO $ MO.withModuleFromAST context (runModuleGen outputFile (generateCode simpleDecls) (Target targetTriple layout)) $ \m -> do
  let opts = PM.defaultCuratedPassSetSpec { PM.optLevel = Just 2 -- TODO optimisation
                                          , PM.simplifyLibCalls = Just True
                                          , PM.useInlinerWithThreshold = Just 225
                                          }
  -- when(optimisation /= 0) $ 
  PM.withPassManager opts $ void . flip PM.runPassManager m
  outputModule targetMachine (MO.File outputFile) outputType m

outputModule :: TargetMachine -> MO.File -> OutputType -> MO.Module -> IO ()
outputModule _  file Raw    m = failInIO $ MO.writeBitcodeToFile file m
outputModule tm file Object m = failInIO $ MO.writeObjectToFile tm file m
outputModule tm (MO.File fileStr) Executable m = withTmpFile $ \obj -> do
  outputModule tm (MO.File obj) Object m
  cc <- getCC
  defs <- (</> "llvm" </> "libidris_rts.a") <$> getDataDir
  exit <- rawSystem cc [obj, defs, "-lm", "-lgmp", "-lgc", "-o", fileStr]
  when (exit /= ExitSuccess) $ ierror "FAILURE: Linking"
outputModule _ _ MavenProject _ = ierror "FAILURE: unsupported output type"

withTmpFile :: (FilePath -> IO a) -> IO a
withTmpFile f = do
  (path, handle) <- tempfile
  hClose handle
  result <- f path
  removeFile path
  return result

failInIO :: ExceptT String IO a -> IO a
failInIO = either fail return <=< runExceptT

ierror :: String -> a
ierror msg = error $ "INTERNAL ERROR: Main: " ++ msg
