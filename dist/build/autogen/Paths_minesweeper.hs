module Paths_minesweeper (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/db2inst1/Desktop/minesweeper/.cabal-sandbox/bin"
libdir     = "/home/db2inst1/Desktop/minesweeper/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/minesweeper-0.1.0.0-FCUX30TWYSP3eJa3y3rmEW"
datadir    = "/home/db2inst1/Desktop/minesweeper/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/minesweeper-0.1.0.0"
libexecdir = "/home/db2inst1/Desktop/minesweeper/.cabal-sandbox/libexec"
sysconfdir = "/home/db2inst1/Desktop/minesweeper/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "minesweeper_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "minesweeper_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "minesweeper_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "minesweeper_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "minesweeper_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
