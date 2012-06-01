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
  , commands = [ Run $ Cpu
                 [ "-L", "3"
                 , "-H", "50"
                 , "--normal", "green"
                 , "--high", "red"
                 ] 10
               , Run $ Memory
                 [ "--template", "Mem: <used>/<total>"] 10
               , Run $ BatteryP ["BAT0"]
                 [ "--template", "<acstatus>[<watts>] (<left>%)"
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
               , Run $ Volume "default" "Master" [ ] 5
               , Run $ MPD
                 [ "--template"
                 , "<statei>: <artist> - <title>"
                 , "--", "-P", withColor "#33ee33" "♫"
                       , "-Z", withColor "#3333ee" "♫"
                       , "-S", withColor "#ee3333" "♫"
                 ] 10
               , Run $ Weather "UUDD"
                 [ "--template", "<tempC>°C <skyCondition>"
                 , "-L", "0"
                 , "-H", "26"
                 , "--low", whiteColor
                 , "--normal", foregroundColor
                 , "--high", orangeColor
                 ] 600
               , Run $ Kbd
                 [ ("ru", withColor blueColorKeyboardLayout "RU")
                 , ("us", withColor redColorKeyboardLayout  "US")
                 ]
               , Run $ StdinReader
               ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "%StdinReader% }{" ++ intercalate separator
    [ "%mpd%"
    , "%default:Master%"
    , "%battery%"
    , "%UUDD%"
    , "%cpu%"
    , "%memory%"
    , "%kbd%"
    , "" -- dummy padding
    ]
  }
  where separator = " " ++ (withColor orangeColor "|") ++ " "
        backgroundColor = blackColor
        foregroundColor = blueColor

withColor ∷ Color → String → String
withColor = printf "<fc=%s>%s</fc>"

blackColor = "#080808"
blueColor = "#2c3c3c"
blueColorKeyboardLayout = "#11eebb"
orangeColor = "#ee9a00"
redColorKeyboardLayout = "#ee7777"
whiteColor = "#9999ff"
