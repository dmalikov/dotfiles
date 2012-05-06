import Control.Applicative ((<$>))
import Control.Monad (liftM2)
import Data.List (isPrefixOf)
import Data.Map (fromList, union)
import System.IO
import XMonad
import XMonad.Actions.SpawnOn (spawnOn)
import XMonad.Actions.UpdatePointer
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
import XMonad.Prompt.Shell
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
      , focusedBorderColor = orangeColor
      , keys               = liftM2 union myKeys (keys defaultConfig)
      , layoutHook         = myLayoutHook
      , logHook            = myLogHook xmproc
      , manageHook         = manageHook defaultConfig <+> myManageHook
      , modMask            = mod4Mask
      , normalBorderColor  = blueColor
      , terminal           = myTerminal
      , workspaces         = myWorkspaces
      }

myKeys conf@(XConfig {modMask = modm}) = fromList $
      [ ( ( modm                , xK_p      ), shellPrompt myXPConfig )
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
      , ( ( modm                , xK_g      ), spawn gvim )
      , ( ( modm                , xK_x      ), spawn recompileXmobar )
      ]

myLogHook :: Handle -> X ()
myLogHook h = do
  updatePointer (Relative 0.5 0.5)
  dynamicLogWithPP $ xmobarPP
    { ppCurrent = xmobarColor orangeColor ""
    , ppOutput  = hPutStrLn h
    , ppSep     = " | "
    , ppSort    = fmap (. scratchpadFilterOutWorkspace) $ ppSort xmobarPP
    , ppTitle   = xmobarColor orangeColor ""
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
    myTiled        = Tall 1 (1/10) (1/2)
    myTwoPaneLeft  = windowNavigation $ combineTwo (TwoPane (1/10) (1/3)) (tabbedAlways shrinkText myTheme) Full
    myTwoPaneRight = windowNavigation $ combineTwo (TwoPane (1/10) (2/3)) Full (tabbedAlways shrinkText myTheme)
    myTabbed       = tabbed shrinkText myTheme

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { font = terminusFont
  , bgColor = blackColor
  , borderColor = blackColor
  , fgColor = orangeColor
  , position = Bottom
  , height = 28
  , autoComplete = Just 1
  }

myTheme = defaultTheme
  { activeColor = darkBlackColor
  , activeBorderColor = darkBlackColor
  , activeTextColor = orangeColor
  , inactiveColor = orangeColor
  , inactiveBorderColor = orangeColor
  , inactiveTextColor = blackColor
  , fontName = terminusFont
  , decoHeight = 26
}

-- Sets the WM name to a given string, so that it could be
-- detected using _NET_SUPPORTING_WM_CHECK protocol
myStartupHook :: X ()
myStartupHook = setWMName "LG3D"

myTerminal = "urxvt -cd ~/"

myWorkspaces  = [ "m", "t", "i", "s" ] ++ map show [5..10]

-- applications
gvim = "gvim"
jws_irssi = "urxvt -title jws -e ssh jws -t 'export LANG=en_US.UTF-8; screen -rD irssi'"
lock_screen = "xscreensaver-command -lock"
mpc_next = "mpc next"
mpc_toggle = "mpc toggle"
recompileXmobar = "ghc --make -O2 ~/.xmonad/xmobar.hs"
volume_decrease = "amixer sset 'Master',0 5%-"
volume_increase = "amixer sset 'Master',0 5%+"
volume_toggle_mute = "amixer set Master toggle"
-- xmobar_run = "xmobar ~/.xmobarrc"
xmobar_run = "~/.xmonad/xmobar"

-- colors
orangeColor = "#ee9a00"
blueColor = "#2c3c3c"
blackColor = "#222222"
darkBlackColor = "#1c1c1c"
whiteColor = "#9999ff"
purpleColor = "#404051"

-- fonts
terminusFont = "-*-terminus-medium-*-*-*-14-*-*-*-*-*-iso10646-*"
