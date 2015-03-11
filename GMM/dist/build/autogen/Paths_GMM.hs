module Paths_GMM (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/julian/.cabal/bin"
libdir     = "/home/julian/.cabal/lib/GMM-0.0.1/ghc-7.6.3"
datadir    = "/home/julian/.cabal/share/GMM-0.0.1"
libexecdir = "/home/julian/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "GMM_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "GMM_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "GMM_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "GMM_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
