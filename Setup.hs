{-# LANGUAGE CPP #-}

import Distribution.Simple
import Distribution.Simple.BuildPaths (autogenModulesDir)
import Distribution.Simple.InstallDirs as I
import Distribution.Simple.LocalBuildInfo as L
import qualified Distribution.Simple.Setup as S
import qualified Distribution.Simple.Program as P
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, rewriteFile)
import Distribution.PackageDescription
import Distribution.Text

import System.FilePath ((</>), splitDirectories,isAbsolute)

-- -----------------------------------------------------------------------------
-- Make Commands

-- use GNU make on FreeBSD
#if defined(freebsd_HOST_OS) || defined(dragonfly_HOST_OS)
mymake = "gmake"
#else
mymake = "make"
#endif
make verbosity =
   P.runProgramInvocation verbosity . P.simpleProgramInvocation mymake


idrisLLVMClean _ flags _ _ = do
        make verbosity [ "-C", "src/rts", "clean", "IDRIS=idris" ]
    where
        verbosity = S.fromFlag $ S.cleanVerbosity flags

idrisLLVMInstall verbosity copy pkg local = do
        installLLVMRTS
    where
        target = datadir $ L.absoluteInstallDirs pkg local copy
        installLLVMRTS = do
            let target' = target </> "rts"
            putStrLn $ "Installing LLVM runtime in " ++ target
            makeInstall "src/rts" target
        makeInstall src target =
            make verbosity [ "-C", src, "install", "TARGET=" ++ target ]

idrisLLVMBuild _ flags _ local = do
        buildLLVM
    where
        verbosity = S.fromFlag $ S.buildVerbosity flags
        buildLLVM = make verbosity ["-C", "src/rts", "build"]

main = defaultMainWithHooks $ simpleUserHooks
    { postClean = idrisLLVMClean
    , postBuild = idrisLLVMBuild
    , postCopy = \_ flags pkg local ->
                   idrisLLVMInstall (S.fromFlag $ S.copyVerbosity flags)
                                    (S.fromFlag $ S.copyDest flags) pkg local
    , postInst = \_ flags pkg local ->
                   idrisLLVMInstall (S.fromFlag $ S.installVerbosity flags)
                                    NoCopyDest pkg local
    }
