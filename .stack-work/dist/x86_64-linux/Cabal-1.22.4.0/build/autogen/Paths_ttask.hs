module Paths_ttask (
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
version = Version [0,0,0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/tune/Documents/Program/Haskell/ttask/.stack-work/install/x86_64-linux/lts-3.9/7.10.2/bin"
libdir     = "/home/tune/Documents/Program/Haskell/ttask/.stack-work/install/x86_64-linux/lts-3.9/7.10.2/lib/x86_64-linux-ghc-7.10.2/ttask-0.0.0.1-72zq9oHyfMzKhn7BjotlNr"
datadir    = "/home/tune/Documents/Program/Haskell/ttask/.stack-work/install/x86_64-linux/lts-3.9/7.10.2/share/x86_64-linux-ghc-7.10.2/ttask-0.0.0.1"
libexecdir = "/home/tune/Documents/Program/Haskell/ttask/.stack-work/install/x86_64-linux/lts-3.9/7.10.2/libexec"
sysconfdir = "/home/tune/Documents/Program/Haskell/ttask/.stack-work/install/x86_64-linux/lts-3.9/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ttask_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ttask_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ttask_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ttask_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ttask_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
