{-# LANGUAGE DataKinds #-}
module Main where

import           Control.Biegunka
import           Control.Biegunka.Templates.HStringTemplate (hStringTemplate)
import           Control.Lens                               (set)
import           Control.Monad                              (void)

import           Environment.Defaults
import qualified Environment.Qumsrc                         as Qumsrc
import qualified Environment.S10                            as S10
import qualified Environment.W540                           as W540

main :: IO ()
main = uncurry rollout =<< runnerOf

rollout a r = void $ r (set templates (hStringTemplate (configs a))) (namespaces a)

configs :: Environment -> Configs
configs Qumsrc = Qumsrc.configs
configs S10    = S10.configs
configs W540   = W540.configs

namespaces :: Environment -> Script 'Sources ()
namespaces Qumsrc = Qumsrc.namespaces
namespaces S10    = S10.namespaces
namespaces W540   = W540.namespaces
