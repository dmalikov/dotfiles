{-# LANGUAGE OverloadedStrings #-}

import Control.Category                -- base
import Control.Scrobbler               -- scrobblers
import Control.Scrobbler.Algorithm.MPD -- scrobblers
import Prelude hiding ((.), id)        -- base


-- Simple scrobbler without fancy stuff
--
-- Each scrobbling step is accompanied with announcement in stdout
main :: IO ()
main = scrobbler $
  -- Tell lastfm about desire to scrobble previous track
  announce . scrobble credentials .
  -- Check if previous track is worth scrobbling
  announce . contest .
  -- Tell lastfm about started tracks
  announce . updateNowPlaying credentials .
  -- Get scrobble candidate from player of choice (MPD here)
  candidate

-- Lastfm credentials. Easy to get with "liblastfm"
credentials :: Credentials
credentials = Credentials
  { apiKey     = "{{E|apiKey}}"
  , sessionKey = "{{E|sessionKey}}"
  , secret     = "{{E|secret}}"
  }

