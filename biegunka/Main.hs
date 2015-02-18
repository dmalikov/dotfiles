{-# LANGUAGE DataKinds #-}
module Main where

import           Control.Biegunka

import           Environment.Defaults
import qualified Environment.S10      as S10
import qualified Environment.W530     as W530

instance Environmentable Environment where
  configs S10  = S10.configs
  configs W530 = W530.configs

  profiles S10  = S10.profiles
  profiles W530 = W530.profiles

main :: IO ()
main = uncurry rollout =<< options [S10, W530]
