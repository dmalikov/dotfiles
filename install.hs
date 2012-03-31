#!/usr/bin/env runhaskell

import Control.Monad (when)
import System.Directory (getHomeDirectory, getCurrentDirectory, removeFile)
import System.FilePath ((</>))
import System.Posix.Files (fileExist, createSymbolicLink)

files =
  [ ( "apvlvrc", ".apvlvrc" )
  , ( "conkyrc", ".conkyrc" )
  , ( "ghci", ".ghci" )
  , ( "gitconfig", ".gitconfig" )
  , ( "gvimrc", ".gvimrc" )
  , ( "mocpconfig", ".moc/config" )
  , ( "mpdconf", ".mpdconf" )
  , ( "ncmpcppconf", ".ncmpcpp/config" )
  , ( "pentadactylrc", ".pentadactylrc" )
  , ( "screenrc", ".screenrc" )
  , ( "vimrc", ".vimrc" )
  , ( "Xdefaults", ".Xdefaults" )
  , ( "xinitrc", ".xinitrc" )
  ]

main :: IO ()
main = mapM addPath files >>= mapM_ (uncurry createSymbolicLinkForce)

addPath :: (FilePath, FilePath) -> IO (FilePath, FilePath)
addPath (file, dest) = do
  homeDir <- getHomeDirectory
  currentDir <- getCurrentDirectory
  return (currentDir </> file, homeDir </> dest)

createSymbolicLinkForce :: FilePath -> FilePath -> IO ()
createSymbolicLinkForce file dest = do
  fileExist dest >>= (flip when) (removeFile dest)
  putStrLn $ "create symlink: " ++ show file ++ " -> " ++ dest
  createSymbolicLink file dest