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
  { apiKey     = "{{OIrLgqSe|dz0mEeiW2pc1SFMtyykAUyLGfJ85ipAfDbnIcPoRIaDKKpL1dFmeoxbh33WfHCyCQ52Rlfbi5+iXZiOnsP66X8/0O/DpuBmX2Ont5GUQt5HkPWHVgXW/B+zaRm4+G51EY74d7tMTzovUoSYlfetl5Irx2f7pv/q9tyLiWdHxW9MRJqzgnJpU99DfHrdy+aKhEtl61GBs5KnP9KlNXysq7wHJ9UFe+B7wz7T1Ab4oDZU7dqbD6yTjL0ZJBTuR4gVOvrMXMLugt7w21XoTKu8DX3oiSaRL/YOytkrfUhIgq7rAvOCtY2d1MMd67Vr58/nosdTGvwYBgGGN8iJxwYns95k85T1jfqIRlLHbATYtk0ol/cCKdxb10Pgr4zbSxpbBb5SAd4bCEPYYDZoUHfdYa6HSRaoR1uO67FJy4K/KSKeHJmxjuj+YMJcQI6OVv20t0/sFyWTct7ayPjGnqVC77Psm6AKwi2SqPTTFmwlfG+CBZ5tcVoZPBoHJHBJ4fP8BcluJTwPf2GFv4E/ZLpnhzL2NKfinzrgKgVe5Mnx1Qdr/DnIPrW2I1R3ZI4MJPg6wXDmo3hQW3fdNbMFmFaXGJYcFRnXOpmGeFsh+NWie67XL1tHlPVUMkyBCrB2gr0tWcj3r6V8l2os7CV7/bHP6QQkArf+Zu20f3uwybJrzufQ=|0pl+fw/LUEJNasI02JTG7lSH9EQIs3MCzVUwlTyav58=}}"
  , sessionKey = "{{OIrLgqSe|yy5DFHqgzEJFsR7RnIavo4p+P+JG2FtsI9aO67aT16laWjSq/fCJJIvB1WIzPanskiylUzHNfaMf+6WuG7c0m8mtTFAxwDqQ9ssJPkT3YV9Zd3wOVOSKvvp+sB8UE0zea322bBbcoU4UFttplFRca4gQDkL8T0xz9QyyrjhW2dSHdlGdS2NnJGOcbK7qBaSvQ6szlLMjHJZc4z3pe1uMipHGakARzgsy9kv29P6vgcAz2agKKpVq3pv5Hs+mKlr0DLPtWRN3+qSjdGCwoNv/KJKjvGS+m4X/gBdo2Y0dHIbCW4+FFMdzSy1U29CByhp0fm0BKXcsZEksAZFtLDXrdEp68r42J86mhCJdA98fYQiV+wbHtd1Wrl2wCaFADd+yNQGnfqQ8Rk+uD6ULQ5ToWgdZldTJZ4cgmbT4C+7rIKaCn8W7p435VJ5WWEy/7/71/agF3iQt/PavBR6tO2VdWfwAlQV1lAeCwJ8BUfYg5vuVG+++WGUQQSDe0F/kvlprp756O6lZfioQIlAl0M7g6BCmG2D4EJNoiJy5Y5tW05OyjDMvnxB4sFEQNOhXtwYaIYYQ3L6mc36K/qZbEJ4B7J0hMiWhogjXUzPYLAijpqCX0idTeGbCQQ6JRfNU9i+1v8OLt5OkQf1ai2u64DKG+veDgvidRpiWY/fgbbCSSz0=|H7kHqXG28HZIzUg72cYhLOp10oBVMe89KNDHJ2IKlEc=}}"
  , secret     = "{{OIrLgqSe|dUmnnYtlKGxQCo+yYP8Z/UQXK5yyKEWBv7sN7IJ/VYY9lxRSKYwZO/tynsX2ebekpfl/17VQqZcIANc8aJ5pOMrCZ/XOUfsONR1KfnzxbsnfPavxRYQQ37YGzIy4UamSehoJ6qnwG2+iQ4dTBJXmaF7oCca7g5MKVtqXll/h7Lx8wesAO4IkKOec5NMxoxteb+QDdCjuMlaaGb8gxkJBQE7E2nCsJvkxM445blkDp/uvJ/gFL5qr5dSPSSeZwJ08C/1KRCt6EUWBLF2cMBy+YMzazryLpD2ALMl0GzIkeFYB8UyXBS8DDy61dyb9usTsNzw9FBjnILBjPWDl3yzOy4VZG4XflPzrLoCyjF+cmKLsO97OVGaTyUScuF1jTYfMG0FQVlJrlMC6mTj8OhcNXU5apzMnlab+v+Is/VCxvyPRvuGQf1kmH7zRKeU071xp4hG++lWqIREFuslofoveYrjUPnTS7BLMTFI2LfuxKZ2D4rItnf8BWrl3ED0YdZlfYe3rsggfVGAN8pFAc4x0GZinGEK9AC6pNwxGi0JU82MITurEI53cuv5eXjWRVJFBIx22fJ6fUbOf4+Pqq7LrTjbKEg4cUJn8pvGJnBkyxcSzOCZwXvDw91KqnGrledXW8+9VCihoZvh03TPVWfJGQ8CZ1yon63YI+/L7g2CqdQk=|upwOriSDgc1SeRw3XpdY4M7XMN/I4ZFuwdKaZ+HNGho=}}"
  }

