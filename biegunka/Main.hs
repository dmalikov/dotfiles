{-# LANGUAGE DataKinds #-}
module Main where

import           Control.Biegunka

import           Environment.Defaults
import qualified Environment.S10      as S10
import qualified Environment.W530     as W530
import qualified Environment.X220     as X220

instance Environmentable Environment where
  configs S10  = S10.configs
  configs X220 = X220.configs
  configs W530 = W530.configs

  profiles S10  = S10.profiles
  profiles X220 = X220.profiles
  profiles W530 = W530.profiles

main :: IO ()
main = uncurry rollout =<< options [X220, S10, W530]
