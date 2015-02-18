{-# LANGUAGE DataKinds #-}
module Main where

import           Control.Biegunka

import           Environment.Defaults
import qualified Environment.S10      as S10

instance Environmentable Environment where
  configs S10  = S10.configs

  profiles S10  = S10.profiles

main :: IO ()
main = uncurry rollout =<< options [S10]
