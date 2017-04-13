{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_haxal (
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

bindir     = "/home/emma/Documents/haskell/project_dbuckman_emsu_kyro-proj/.stack-work/install/x86_64-linux/lts-8.9/8.0.2/bin"
libdir     = "/home/emma/Documents/haskell/project_dbuckman_emsu_kyro-proj/.stack-work/install/x86_64-linux/lts-8.9/8.0.2/lib/x86_64-linux-ghc-8.0.2/haxal-0.1.0.0-5RTt9dmOvEGLcGJVgwIJJO"
dynlibdir  = "/home/emma/Documents/haskell/project_dbuckman_emsu_kyro-proj/.stack-work/install/x86_64-linux/lts-8.9/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/emma/Documents/haskell/project_dbuckman_emsu_kyro-proj/.stack-work/install/x86_64-linux/lts-8.9/8.0.2/share/x86_64-linux-ghc-8.0.2/haxal-0.1.0.0"
libexecdir = "/home/emma/Documents/haskell/project_dbuckman_emsu_kyro-proj/.stack-work/install/x86_64-linux/lts-8.9/8.0.2/libexec"
sysconfdir = "/home/emma/Documents/haskell/project_dbuckman_emsu_kyro-proj/.stack-work/install/x86_64-linux/lts-8.9/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haxal_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haxal_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haxal_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haxal_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haxal_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haxal_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
