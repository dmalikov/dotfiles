module Main where

import qualified Environment.T510 as T510
import qualified Environment.X220 as X220

import Control.Lens (set)
import Data.Monoid (mempty)
import Options.Applicative

import Biegunka

data Environment = X220 | T510

data Conf = Conf
  { enable_pretend :: Bool
  , enable_verify :: Bool
  , environment :: Environment
  }

run_with_conf :: Conf -> IO ()
run_with_conf c = do
  let pretend' = if enable_pretend c then pretend <> pause else mempty
      verify' = if enable_verify c then verify else mempty
      settings' = case (environment c) of
                    T510 -> T510.settings
                    X220 -> X220.settings
      profiles' = case environment c of
                    T510 -> T510.profiles
                    X220 -> X220.profiles
  biegunka (set root "~") (pretend' <> execute (set templates $ Templates settings') <> verify') profiles'


main :: IO ()
main = execParser opts >>= run_with_conf
 where
   opts = info parser mempty
   parser = Conf
     <$> switch (long "pretend" <> help "enable pretending: show what will be done")
     <*> switch (long "verify" <> help "enable verifying: check what biegunka done")
     <*> ( flag' X220 (long "x220" <> help "use x220 as an active environment")
       <|> flag' T510 (long "t510" <> help "use t510 as an active environment"))
