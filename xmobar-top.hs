{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Application
import Text.Printf

type Color = String

main :: IO ()
main = xmobar Config
  { hideOnStart = False
  , persistent = True
  , lowerOnStart = False
  , font = "-*-terminus-medium-r-normal--12-*"
  , border = NoBorder
  , borderColor = backgroundColor
  , bgColor = backgroundColor
  , fgColor = foregroundColor
  , position = TopSize C 100 20
  , commands = [ Run $ TopMem
     [ "--template", concatMap nameAndMem [1..10]
     ] 30
     , Run $ TopProc
     [ "--template", concatMap nameAndCpu [1..10]
     ] 30
   ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "%topmem% }{ %top%"
  }
 where
  backgroundColor = blackColor
  foregroundColor = blueColor
  (nameAndMem ∷ Int → String) = \n → concat
    [ withColor blueColor "("
    , withColor blueColorKeyboardLayout $ printf "<name%d> " n
    , withColor yellowColor $ printf "<mem%d>" n
    , withColor blueColor ") "
    ]
  (nameAndCpu ∷ Int → String) = \n → concat
    [ withColor blueColor "("
    , withColor blueColorKeyboardLayout $ printf "<name%d> " n
    , withColor yellowColor $ printf "<cpu%d>" n
    , withColor blueColor ") "
    ]


withColor ∷ Color → String → String
withColor = printf "<fc=%s>%s</fc>"

blackColor = "#080808"
blueColor = "#2c3c3c"
blueColorKeyboardLayout = "#11eebb"
yellowColor = "#ffd317"

