{-# LANGUAGE DataKinds #-}
module Main where

import           Control.Biegunka

import           Environment.Defaults
import qualified Environment.Qumsrc   as Qumsrc
import qualified Environment.S10      as S10
import qualified Environment.W540     as W540

instance Environmentable Environment where
  configs Qumsrc = Qumsrc.configs
  configs S10    = S10.configs
  configs W540   = W540.configs

  profiles Qumsrc = Qumsrc.profiles
  profiles S10    = S10.profiles
  profiles W540   = W540.profiles

main :: IO ()
main = uncurry rollout =<< options [Qumsrc, S10, W540]
