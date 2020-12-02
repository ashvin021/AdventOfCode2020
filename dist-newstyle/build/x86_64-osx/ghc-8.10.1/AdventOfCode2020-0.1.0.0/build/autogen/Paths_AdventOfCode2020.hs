{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_AdventOfCode2020 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/ashvin/.cabal/bin"
libdir     = "/Users/ashvin/.cabal/lib/x86_64-osx-ghc-8.10.1/AdventOfCode2020-0.1.0.0-inplace"
dynlibdir  = "/Users/ashvin/.cabal/lib/x86_64-osx-ghc-8.10.1"
datadir    = "/Users/ashvin/.cabal/share/x86_64-osx-ghc-8.10.1/AdventOfCode2020-0.1.0.0"
libexecdir = "/Users/ashvin/.cabal/libexec/x86_64-osx-ghc-8.10.1/AdventOfCode2020-0.1.0.0"
sysconfdir = "/Users/ashvin/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "AdventOfCode2020_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "AdventOfCode2020_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "AdventOfCode2020_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "AdventOfCode2020_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "AdventOfCode2020_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "AdventOfCode2020_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
