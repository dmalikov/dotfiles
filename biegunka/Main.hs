{-# LANGUAGE DataKinds, TemplateHaskell #-}
module Main where

import qualified Environment.X220 as X220

import Control.Lens (set)

import Control.Biegunka
import Control.Biegunka.Templates.HStringTemplate

data Environment = X220 | MacBookPro

makeOptionsParser ''Environment

main :: IO ()
main = do
  (env, r) <- optionsParser
  case env of
    X220 -> r (set root "~" . set templates (hStringTemplate X220.settings)) X220.profiles
    MacBookPro -> error "not ready yet"
