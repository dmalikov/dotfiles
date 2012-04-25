import Control.Applicative ((<$>))
import Control.Monad (liftM2)
import Data.List (isPrefixOf)
import Data.Map (fromList, union)
import System.IO
import XMonad
import XMonad.Actions.SpawnOn (spawnOn)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Combo
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad

import qualified XMonad.Actions.CycleWS as CWS
import qualified XMonad.StackSet as W

main :: IO ()
main = do
    xmproc <- spawnPipe xmobar_run
    xmonad $ defaultConfig
      { borderWidth        = 1
      , focusFollowsMouse  = False
      , focusedBorderColor = "#ee9a00"
      , keys               = liftM2 union myKeys (keys defaultConfig)
      , layoutHook         = myLayoutHook
      , logHook            = myLogHook xmproc
      , manageHook         = manageHook defaultConfig <+> myManageHook
      , modMask            = mod4Mask
      , normalBorderColor  = "#2c3c3b"
      , terminal           = myTerminal
      , workspaces         = myWorkspaces
      }

myKeys conf@(XConfig {modMask = modm}) = fromList $
      [ ( ( modm                , xK_p      ), spawn dmenu_run )
      , ( ( modm .|. controlMask, xK_l      ), spawn lock_screen )
      , ( ( modm .|. controlMask, xK_9      ), spawn volume_decrease )
      , ( ( modm .|. controlMask, xK_0      ), spawn volume_increase )
      , ( ( modm .|. controlMask, xK_m      ), spawn volume_toggle_mute )
      , ( ( modm .|. controlMask, xK_j      ), spawnOn "t" jws_irssi )
      , ( ( modm .|. controlMask, xK_p      ), spawn mpc_toggle )
      , ( ( modm .|. controlMask, xK_period ), spawn mpc_next )
      , ( ( modm                , xK_Right  ), CWS.nextWS )
      , ( ( modm                , xK_Left   ), CWS.prevWS )
      , ( ( modm                , xK_s      ), scratchpadSpawnAction conf )
      ]

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ xmobarPP
  { ppCurrent = xmobarColor "#ee9a00" ""
  , ppOutput  = hPutStrLn h
  , ppSep     = " | "
  , ppSort    = fmap (.scratchpadFilterOutWorkspace) $ ppSort xmobarPP
  , ppTitle   = xmobarColor "#ee9a00" ""
  }

myManageHook :: ManageHook
myManageHook = scratchpadHook <+> (composeAll $
  [ myIgnores --> doIgnore
  , myFloats  --> doFloat
  ] )
  where
    myFloats = foldr1 (<||>)
      [ ("Figure" `isPrefixOf`) <$> title
      , className =? "feh"
      , className =? "gimp"
      , className =? "Virtual Box"
      ]
    myIgnores = foldr1 (<||>)
      [ resource =? "panel"
      , resource =? "trayer"
      ]

scratchpadHook = scratchpadManageHook (W.RationalRect paddingLeft paddingTop width height')
  where
    height'     = 0.7
    width       = 0.6
    paddingTop  = 0.2
    paddingLeft = (1 - width) / 2

myLayoutHook = smartBorders . avoidStruts $
  myTiled ||| myTwoPaneLeft ||| myTwoPaneRight ||| myTabbed
  where
    myTiled        = Tall 1 (3/100) (1/2)
    myTwoPaneLeft  = windowNavigation $ combineTwo (TwoPane (3/100) (2.6/10)) (tabbedAlways shrinkText defaultTheme) Full
    myTwoPaneRight = windowNavigation $ combineTwo (TwoPane (3/100) (7.4/10)) Full (tabbedAlways shrinkText defaultTheme)
    myTabbed       = tabbed shrinkText defaultTheme

-- Sets the WM name to a given string, so that it could be
-- detected using _NET_SUPPORTING_WM_CHECK protocol
myStartupHook :: X ()
myStartupHook = setWMName "LG3D"

myTerminal = "urxvt -cd ~/"

myWorkspaces  = [ "m", "t", "i", "s" ] ++ map show [5..10]

myXPrompt :: XPConfig
myXPrompt = defaultXPConfig { font = "terminus-medium:weight=normal:pixelsize=16" }

-- applications
dmenu_run = "dmenu_run -b -i -fn '-*-terminus-medium-*-*-*-20-*-*-*-*-*-iso10646-*' -nb '#222222' -nf '#ee9a00' -sb '#404051' -sf '#9999ff'"
jws_irssi = "urxvt -title jws -e ssh jws -t 'export LANG=en_US.UTF-8; screen -rD irssi'"
lock_screen = "xscreensaver-command -lock"
mpc_next = "mpc next"
mpc_toggle = "mpc toggle"
volume_decrease = "amixer sset 'Master',0 5%-"
volume_increase = "amixer sset 'Master',0 5%+"
volume_toggle_mute = "amixer set Master toggle"
xmobar_run = "xmobar ~/.xmobarrc"
