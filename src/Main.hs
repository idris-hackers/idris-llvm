module Main where

import Idris.Core.TT
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.REPL

import Paths_idris_llvm

import IRTS.Compiler
import IRTS.CodegenLLVM
import qualified IRTS.CodegenCommon as CG

import System.Environment
import System.Exit

data Opts = Opts { inputs :: [FilePath],
                   output :: FilePath,
                   oTargetTriple :: String,
                   oTargetCPU :: String }

showUsage = do putStrLn "Usage: idris-llvm <ibc-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             triple <- return $ ""
             cpu <- return $ ""
             return $ process (Opts [] "a.out" triple cpu) xs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

llvm_main :: Opts -> Idris ()
llvm_main opts = do elabPrims
                    loadInputs (inputs opts) Nothing
                    mainProg <- elabMain
                    ir <- compile (Via "llvm") (output opts) (Just mainProg)
                    runIO $ codegenLLVM (ir { CG.targetTriple = oTargetTriple opts, CG.targetCPU = oTargetCPU opts } )

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts)) 
             then showUsage
             else runMain (llvm_main opts)

