#!/usr/bin/env runhaskell

import Control.Monad (when)
import System.Directory (getHomeDirectory, getCurrentDirectory, removeFile)
import System.FilePath ((</>))
import System.Posix.Files (fileExist, createSymbolicLink)

files =
  [
    ( "ackrc", ".ackrc" )
  , ( "apvlvrc", ".apvlvrc" )
  , ( "buftabs.js", ".pentadactyl/plugins/buftabs.js" )
  , ( "conkyrc", ".conkyrc" )
  , ( "ghci", ".ghci" )
  , ( "gitconfig", ".gitconfig" )
  , ( "gitignore", ".gitignore" )
  , ( "gvimrc", ".gvimrc" )
  , ( "gtkrc-2.0.mine", ".gtkrc-2.0.mine" )
  , ( "haskell.vim", ".vim/after/syntax/haskell.vim" )
  , ( "mocpconfig", ".moc/config" )
  , ( "mpdconf", ".mpdconf" )
  , ( "ncmpcppconf", ".ncmpcpp/config" )
  , ( "pentadactylrc", ".pentadactylrc" )
  , ( "pemees.penta", ".pentadactyl/color/pemees.penta" )
  , ( "screenrc", ".screenrc" )
  , ( "shiva.Xresources", "shiva.Xresources" )
  , ( "tigrc", ".tigrc" )
  , ( "vimrc", ".vimrc" )
  , ( "Xdefaults", ".Xdefaults" )
  , ( "XCompose", ".XCompose" )
  , ( "xinitrc", ".xinitrc" )
  , ( "xmobarrc", ".xmobarrc" )
  , ( "xmobar.hs", ".xmonad/xmobar.hs" )
  , ( "xmonad.hs", ".xmonad/xmonad.hs" )
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
