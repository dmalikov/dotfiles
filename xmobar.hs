module Main
  ( main
  ) where

import Application
import Data.List (intercalate)

main :: IO ()
main = xmobar Config
  { lowerOnStart = False
  , font = "-*-terminus-medium-r-normal--12-*"
  , border = NoBorder
  , borderColor = "#1c1c1c"
  , bgColor = "#1c1c1c"
  , fgColor = "#8080A1"
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
                 , "-O", "<fc=#429942>On</fc> - "
                 , "-o", ""
                 , "-L", "-15"
                 , "-H", "-5"
                 , "-l", "red"
                 , "-m", "blue"
                 , "-h", "#429942"
                 , "-c", "charge_full_design"
                 ] 200
               , Run $ Date "/ %H:%M" "date" 10
               , Run $ Volume "default" "Master" [ ] 5
               , Run $ StdinReader
               ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = intercalate separator
    [ "%StdinReader% }{ %default:Master%"
    , "%battery%"
    , "%cpu%"
    , "%memory%"
    -- , "%wlan0% <fc=#ee9a00>%date%</fc> "
    , "%wlan0% <fc=#ffffff>%date%</fc> "
    ]
  }  where separator = " <fc=#ee9a00>|</fc> "
  


