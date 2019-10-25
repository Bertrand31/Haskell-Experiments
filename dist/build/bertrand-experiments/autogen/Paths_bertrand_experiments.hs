{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_bertrand_experiments (
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

bindir     = "/home/bertrand/.cabal/bin"
libdir     = "/home/bertrand/.cabal/lib/x86_64-linux-ghc-8.4.4/bertrand-experiments-0.1.0.0-Kv4GbPctcN9v1cjPFHHHq-bertrand-experiments"
dynlibdir  = "/home/bertrand/.cabal/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/home/bertrand/.cabal/share/x86_64-linux-ghc-8.4.4/bertrand-experiments-0.1.0.0"
libexecdir = "/home/bertrand/.cabal/libexec/x86_64-linux-ghc-8.4.4/bertrand-experiments-0.1.0.0"
sysconfdir = "/home/bertrand/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bertrand_experiments_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bertrand_experiments_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "bertrand_experiments_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "bertrand_experiments_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bertrand_experiments_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bertrand_experiments_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
