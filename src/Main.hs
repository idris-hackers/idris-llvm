module Main where

import Idris.Core.TT
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.REPL

import IRTS.Compiler
import IRTS.CodegenLLVM
import qualified IRTS.CodegenCommon as CG

import LLVM.General.Target

import System.Environment
import System.Exit

import Paths_idris

data Opts = Opts { inputs :: [FilePath],
                   output :: FilePath,
                   oTargetTriple :: String,
                   oTargetCPU :: String }

showUsage = do putStrLn "Usage: idris-llvm <ibc-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             triple <- getDefaultTargetTriple
             cpu <- getHostCPUName
             return $ process (Opts [] "a.out" triple cpu) xs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

llvm_main :: Opts -> Idris ()
llvm_main opts = do elabPrims
                    loadInputs (inputs opts) Nothing
                    mainProg <- elabMain
                    ir <- compile (Via "llvm") (output opts) mainProg
                    runIO $ codegenLLVM (ir { CG.targetTriple = oTargetTriple opts, CG.targetCPU = oTargetCPU opts } )

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts)) 
             then showUsage
             else runMain (llvm_main opts)



