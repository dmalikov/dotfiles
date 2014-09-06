{-# LANGUAGE DataKinds, DeriveDataTypeable #-}
module Main where

import qualified Environment.X220 as X220
import qualified Environment.S10 as S10

import Control.Lens (set)

import Control.Biegunka
import Control.Biegunka.Templates.HStringTemplate

data Environment = X220 | MacBookPro | S10
  deriving (Data, Typeable)

main :: IO ()
main = do
  (env, r) <- options [X220, MacBookPro, S10]
  case env of
    X220 -> r (set root "~" . set templates (hStringTemplate X220.settings)) X220.profiles
    S10 -> r (set root "~" . set templates (hStringTemplate S10.settings)) S10.profiles
    MacBookPro -> error "not ready yet"
