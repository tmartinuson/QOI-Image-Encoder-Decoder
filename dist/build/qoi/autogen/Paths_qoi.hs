{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_qoi (
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

bindir     = "/home/tmart/.cabal/bin"
libdir     = "/home/tmart/.cabal/lib/x86_64-linux-ghc-8.6.5/qoi-0.1.0.0-HlzWoiXiEmnCWh9lUcbg7N"
dynlibdir  = "/home/tmart/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/tmart/.cabal/share/x86_64-linux-ghc-8.6.5/qoi-0.1.0.0"
libexecdir = "/home/tmart/.cabal/libexec/x86_64-linux-ghc-8.6.5/qoi-0.1.0.0"
sysconfdir = "/home/tmart/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "qoi_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "qoi_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "qoi_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "qoi_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "qoi_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "qoi_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
