module Paths_GMM (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/julian/.cabal/bin"
libdir     = "/home/julian/.cabal/lib/x86_64-linux-ghc-7.6.3/GMM-0.0.1"
datadir    = "/home/julian/.cabal/share/x86_64-linux-ghc-7.6.3/GMM-0.0.1"
libexecdir = "/home/julian/.cabal/libexec"
sysconfdir = "/home/julian/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "GMM_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "GMM_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "GMM_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "GMM_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "GMM_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
