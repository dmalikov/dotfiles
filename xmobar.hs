{-# LANGUAGE UnicodeSyntax #-}

module Main
  ( main
  ) where

import Application
import Data.List (intercalate)
import Text.Printf

type Color = String

main :: IO ()
main = xmobar Config
  { lowerOnStart = False
  , font = "-*-terminus-medium-r-normal--12-*"
  , border = NoBorder
  , borderColor = backgroundColor
  , bgColor = backgroundColor
  , fgColor = foregroundColor
  , position = Static { xpos = 120
                      , ypos = 1060
                      , width = 1800
                      , height = 20
                      }
  , commands = [ Run $ Network "wlan0"
                 [ "-L", "0"
                 , "-H", "32"
                 , "--normal", "#429942"
                 , "--high", "#A36666"
                 ] 10
               , Run $ Cpu
                 [ "-L", "3"
                 , "-H", "50"
                 , "--normal", "green"
                 , "--high", "red"
                 ] 10
               , Run $ Memory ["-t","Mem: <usedratio>%"] 10
               , Run $ BatteryP ["BAT0"]
                 [ "-t", "<acstatus><watts> (<left>%)"
                 , "-L", "10"
                 , "-H", "80"
                 , "-p", "3"
                 , "--"
                 , "-L", "-15"
                 , "-H", "-5"
                 , "-l", "red"
                 , "-m", "blue"
                 , "-h", "#429942"
                 ] 200
               , Run $ Date "/ %H:%M" "date" 10
               , Run $ Volume "default" "Master" [ ] 5
               , Run $ MPD
                 [ "--template"
                 , "<statei>: <artist> - <title>"
                 , "--", "-P", withColor "#33ee33" "♫"
                       , "-Z", withColor "#3333ee" "♫"
                       , "-S", withColor "#ee3333" "♫"
                 ] 10
               , Run $ StdinReader
               ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "%StdinReader% }{" ++ intercalate' separator
    [ "%mpd%"
    , "%default:Master%"
    , "%battery%"
    , "%cpu%"
    , "%memory%"
    , "%wlan0%" ++ withColor "#ee9a00" "%date% "
    ]
  }
  where intercalate' s list = s ++ intercalate s list
        separator = " " ++ (withColor "#ee9a00" "|") ++ " "
        backgroundColor = "#080808"
        foregroundColor = "#8080a1"

withColor ∷ Color → String → String
withColor = printf "<fc=%s>%s</fc>"
