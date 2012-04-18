import System.IO
import XMonad
import XMonad.Actions.SpawnOn (spawnOn)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Combo
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)

import qualified XMonad.Actions.CycleWS as CWS

main = do
    xmproc <- spawnPipe xmobar_run
    xmonad $ defaultConfig
      { startupHook        = myStartupHook
      , modMask            = mod4Mask
      , terminal           = myTerminal
      , workspaces         = myWorkspaces
      , layoutHook         = myLayoutHook
      , borderWidth        = 1
      , normalBorderColor  = "#2c3c3b"
      , focusedBorderColor = "#ee9a00"
      , focusFollowsMouse  = False
      , manageHook         = manageHook defaultConfig <+> myManageHook
      , logHook            = dynamicLogWithPP $ xmobarPP
        { ppOutput  = hPutStrLn xmproc
        , ppCurrent = xmobarColor "#ee9a00" ""
        , ppTitle   = xmobarColor "#ee9a00" ""
        , ppSep     = " | "
        }
      } `additionalKeys`
      [ ( ( mod4Mask                , xK_p      ), spawn dmenu_run )
      , ( ( mod4Mask .|. controlMask, xK_l      ), spawn lock_screen )
      , ( ( mod4Mask .|. controlMask, xK_9      ), spawn volume_decrease )
      , ( ( mod4Mask .|. controlMask, xK_0      ), spawn volume_increase )
      , ( ( mod4Mask .|. controlMask, xK_m      ), spawn volume_toggle_mute )
      , ( ( mod4Mask .|. controlMask, xK_j      ), spawnOn "t" jws_irssi )
      , ( ( mod4Mask .|. controlMask, xK_p      ), spawn mpc_toggle )
      , ( ( mod4Mask .|. controlMask, xK_period ), spawn mpc_next )
      , ( ( mod4Mask                , xK_Right  ), CWS.nextWS )
      , ( ( mod4Mask                , xK_Left   ), CWS.prevWS )
      ]

-- Sets the WM name to a given string, so that it could be
-- detected using _NET_SUPPORTING_WM_CHECK protocol
myStartupHook = setWMName "LG3D"

myTerminal = "urxvt -cd ~/"

myWorkspaces  = [ "m", "t", "i", "s" ] ++ map show [5..10]

myManageHook = composeAll . concat $
    [ [ className =? c --> doFloat  | c <- myFloats]
    , [ resource  =? r --> doIgnore | r <- myIgnores]
    ]
  where myIgnores = [ "panel", "trayer" ]
        myFloats  = [ "feh", "gimp", "VirtualBox" ]

myLayoutHook = smartBorders . avoidStruts . layoutHints $
  myTiled ||| myTwoPaneLeft ||| myTwoPaneRight ||| myTabbed
  where
    myTiled        = Tall 1 (3/100) (1/2)
    myTwoPaneLeft  = windowNavigation $ combineTwo (TwoPane (3/100) (2.6/10)) (tabbedAlways shrinkText defaultTheme) Full
    myTwoPaneRight = windowNavigation $ combineTwo (TwoPane (3/100) (7.4/10)) Full (tabbedAlways shrinkText defaultTheme)
    myTabbed       = tabbed shrinkText defaultTheme

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
