{-# LANGUAGE DataKinds #-}
module Main where

import           Control.Biegunka
import           Control.Biegunka.Templates.HStringTemplate (hStringTemplate)
import           Control.Lens (set)
import           Control.Monad (void)

import           Environment.Defaults
import qualified Environment.Lnd as Lnd
import qualified Environment.Qumsrc as Qumsrc
import qualified Environment.S10 as S10
import qualified Environment.Sb2 as Sb2
import qualified Environment.W540 as W540

main :: IO ()
main = uncurry rollout =<< runnerOf

rollout a r = void $ r (set templates (hStringTemplate (configs a))) (namespaces a)

configs :: Environment -> Configs
configs Lnd    = Lnd.configs
configs Qumsrc = Qumsrc.configs
configs S10    = S10.configs
configs Sb2    = Sb2.configs
configs W540   = W540.configs

namespaces :: Environment -> Script 'Sources ()
namespaces Lnd    = Lnd.namespaces
namespaces Qumsrc = Qumsrc.namespaces
namespaces S10    = S10.namespaces
namespaces Sb2    = Sb2.namespaces
namespaces W540   = W540.namespaces
