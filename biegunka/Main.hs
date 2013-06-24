{-# LANGUAGE DataKinds, TemplateHaskell #-}
module Main where

import qualified Environment.T510 as T510
import qualified Environment.X220 as X220

import Control.Lens (set)

import Biegunka

data Environment = X220 | T510

makeOptionsParser ''Environment

main :: IO ()
main = do
  (env, run) <- optionsParser
  case env of
    X220 -> run (set root "~") (set templates $ Templates X220.settings) X220.profiles
    T510 -> run (set root "~") (set templates $ Templates T510.settings) T510.profiles
