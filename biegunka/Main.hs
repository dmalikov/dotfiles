module Main where

import qualified Environment.T510 as T510
import qualified Environment.X220 as X220

import Control.Lens (set)
import Data.Monoid (mempty)
import Options.Applicative

import Biegunka

data Conf = Conf
  { enable_pretend :: Bool
  , enable_verify :: Bool
  , environment :: String
  }

run_with_conf :: Conf -> IO ()
run_with_conf c = do
  let pretend' = if enable_pretend c then pretend <> pause else mempty
      verify' = if enable_verify c then verify else mempty
      settings' = case (environment c) of
                    "t510" -> T510.settings
                    "x220" -> X220.settings
                    e -> error $ "no such environment " ++ e
      profiles' = case environment c of
                    "t510" -> T510.profiles
                    "x220" -> X220.profiles
                    e -> error $ "no such environment " ++ e
  biegunka (set root "~") (pretend' <> execute (set templates $ Templates settings') <> verify') profiles'


main :: IO ()
main = execParser opts >>= run_with_conf
 where
   opts = info parser mempty
   parser = Conf
     <$> switch (long "pretend" <> help "enable pretending: show what will be done")
     <*> switch (long "verify" <> help "enable verifying: check what biegunka done")
     <*> strOption (long "environment" <> short 'e' <> metavar "ENV" <> help "set current environment: x220 or t510")
