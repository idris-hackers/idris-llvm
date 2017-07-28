module Main where

import Idris.Core.TT
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.Main
import Idris.Options
import Idris.REPL

import Paths_idris_llvm

import IRTS.Compiler
import IRTS.CodegenLLVM
import qualified IRTS.CodegenCommon as CG

import LLVM.Target
import System.Environment
import System.Exit

data Opts = Opts { inputs :: [FilePath],
                   output :: FilePath,
                   outputType :: CG.OutputType,
                   oTargetTriple :: String,
                   oTargetCPU :: String }

showUsage = do putStrLn "Usage: idris-llvm <ibc-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts [] "a.out"  CG.Executable "" "") xs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts ("-S":xs) = process (opts { outputType = CG.Raw }) xs
    process opts ("-c":xs) = process (opts { outputType = CG.Object }) xs
    process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

llvm_main :: Opts -> Idris ()
llvm_main opts = do elabPrims
                    loadInputs (inputs opts) Nothing
                    mainProg <- elabMain
                    ir <- compile (Via IBCFormat "llvm") (output opts) (Just mainProg)
                    runIO $ putStrLn (show $ CG.outputType ir)
                    runIO $ codegenLLVM (ir {
                         CG.targetTriple = oTargetTriple opts,
                         CG.targetCPU = oTargetCPU opts,
                         CG.outputType = outputType opts } )

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts)) 
             then showUsage
             else runMain (llvm_main opts)

