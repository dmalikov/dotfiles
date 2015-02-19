{-# LANGUAGE DataKinds #-}
module Main where

import           Control.Biegunka

import           Environment.Defaults
import qualified Environment.W540     as W540
import qualified Environment.S10      as S10

instance Environmentable Environment where
  configs W540 = W540.configs
  configs S10  = S10.configs

  profiles W540 = W540.profiles
  profiles S10  = S10.profiles

main :: IO ()
main = uncurry rollout =<< options [W540, S10]
