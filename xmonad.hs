{-# LANGUAGE UnicodeSyntax #-}
import Control.Applicative ((<$>))
import Control.Monad (liftM2)
import Data.List (isPrefixOf)
import Data.Map (fromList, union)
import Data.Maybe (isJust)
import Data.Time (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Directory (getHomeDirectory, renameFile)
import System.FilePath.Posix ((<.>), joinPath)
import System.IO (Handle, hPutStrLn)
import System.Locale (defaultTimeLocale)
import XMonad
import XMonad.Actions.SpawnOn (spawnOn)
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Combo
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad
-- import XMonad.Util.WorkspaceScreenshot

import qualified XMonad.Actions.CycleWS as CWS
import qualified XMonad.StackSet as S

main :: IO ()
main = do
  -- initCapturing
  xmproc <- spawnPipe xmobar_run
  xmonad $ defaultConfig
    { borderWidth        = 7
    , focusFollowsMouse  = False
    , focusedBorderColor = orangeDarkestColor
    , keys               = liftM2 union myKeys (keys defaultConfig)
    , layoutHook         = myLayoutHook
    , logHook            = myLogHook xmproc
    , manageHook         = manageHook defaultConfig <+> myManageHook
    , modMask            = mod4Mask
    , normalBorderColor  = blackDarkColor
    , terminal           = myTerminal
    , workspaces         = myWorkspaces
    }

myKeys conf@(XConfig {modMask = modm}) = fromList
  [ ( ( modm                , xK_p      ), shellPrompt myXPConfig )
  , ( ( modm .|. controlMask, xK_l      ), spawn lock_screen )
  , ( ( modm .|. controlMask, xK_9      ), spawn volume_decrease )
  , ( ( modm .|. controlMask, xK_0      ), spawn volume_increase )
  , ( ( modm .|. controlMask, xK_m      ), spawn volume_toggle_mute )
  , ( ( modm .|. controlMask, xK_j      ), spawnOn "t" jws_irssi )
  , ( ( modm .|. controlMask, xK_p      ), spawn mpc_toggle )
  , ( ( modm .|. controlMask, xK_period ), spawn mpc_next )
  , ( ( modm                , xK_j      ), CWS.prevWS )
  , ( ( modm                , xK_k      ), CWS.nextWS )
  , ( ( modm                , xK_s      ), scratchpadSpawnAction conf )
  , ( ( modm                , xK_g      ), spawn gvim )
  , ( ( modm                , xK_x      ), spawn recompileXmobar )
  , ( ( modm                , xK_a      ), sendMessage MirrorShrink )
  , ( ( modm                , xK_z      ), sendMessage MirrorExpand )
  , ( ( modm                , xK_m      ), windows S.swapMaster )
  -- , ( ( modm .|. shiftMask  , xK_u      ), captureWorkspacesWhen visible moveToImg horizontally )
  -- , ( ( modm .|. shiftMask  , xK_y      ), captureWorkspacesWhen current moveToImg horizontally )
  -- , ( ( modm .|. shiftMask  , xK_a      ), captureWorkspacesWhen defaultPredicate moveToImg horizontally )
  , ( ( modm                , xK_b      ), CWS.toggleWS' ["NSP"] )
  ]

-- xmonad-screenshoter stuff
visible ∷ WindowSpace → X Bool
visible = return . isJust . S.stack

current ∷ WindowSpace → X Bool
current w = withWindowSet $ (\ws → return $ (S.tag . S.workspace . S.current $ ws) == S.tag w)

moveToImg ∷ FilePath → IO ()
moveToImg filepath = do
  hd ← getHomeDirectory
  date ← formatTime defaultTimeLocale "%F-%X" <$> getCurrentTime
  let newFileName = joinPath [hd, "img", "screen", "own", "xmonad", date] <.> "png"
  renameFile filepath newFileName

myLogHook :: Handle -> X ()
myLogHook h = do
  updatePointer (Relative 0.5 0.5)
  dynamicLogWithPP $ xmobarPP
    { ppCurrent = xmobarColor orangeColor ""
    , ppOutput  = hPutStrLn h
    , ppSep     = " | "
    , ppSort    = fmap (. scratchpadFilterOutWorkspace) $ ppSort xmobarPP
    , ppTitle   = xmobarColor orangeDarkColor ""
    }
  fadeInactiveLogHook 0.7

myManageHook :: ManageHook
myManageHook = scratchpadHook <+> composeAll
  [ myIgnores --> doIgnore
  , myFloats  --> doFloat
  ]
  where
    myFloats = foldr1 (<||>)
      [ ("Figure" `isPrefixOf`) <$> title
      , className =? "feh"
      , className =? "gimp"
      , className =? "Virtual Box"
      , className =? "Git-cola"
      ]
    myIgnores = foldr1 (<||>)
      [ resource =? "panel"
      , resource =? "trayer"
      ]

scratchpadHook = scratchpadManageHook (S.RationalRect paddingLeft paddingTop width height')
  where
    height'     = 0.7
    width       = 0.6
    paddingTop  = 0.2
    paddingLeft = (1 - width) / 2

myLayoutHook = smartBorders . avoidStruts $
  resizableTile ||| myTwoPaneLeft ||| myTwoPaneRight ||| myTabbed
  where
    resizableTile  = ResizableTall 1 (1/30) (1/2) []
    myTwoPaneLeft  = windowNavigation $ combineTwo (TwoPane (1/10) (1/3)) (tabbedAlways shrinkText myTheme) Full
    myTwoPaneRight = windowNavigation $ combineTwo (TwoPane (1/10) (2/3)) Full (tabbedAlways shrinkText myTheme)
    myTabbed       = tabbed shrinkText myTheme

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { font = terminusFont
  , bgColor = blackDarkColor
  , borderColor = blackColor
  , fgColor = blueColor
  , position = Bottom
  , height = 28
  , autoComplete = Just 1
  }

myTheme = defaultTheme
  { activeColor = blackDarkColor
  , activeBorderColor = blackDarkColor
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

myTerminal = urxvt ++ " -cd ~/"

myWorkspaces  = [ "♫", "τ", "Λ" ] ++ map show [4..9] ++ [ "α", "β", "λ", "ρ", "«", "»" ]

-- applications
gvim = "gvim"
jws_irssi = urxvt ++ " -title jεωs -e ssh jws -t 'export LANG=en_US.UTF-8; tmux attach-session -t irssi'"
lock_screen = "xscreensaver-command -lock"
mpc_next = "mpc next"
mpc_toggle = "mpc toggle"
recompileXmobar = "recompile_xmobar"
urxvt = "urxvt -name shiva"
volume_decrease = "amixer sset 'Master',0 5%-"
volume_increase = "amixer sset 'Master',0 5%+"
volume_toggle_mute = "amixer set Master toggle"
xmobar_run = "~/.xmonad/xmobar"

-- colors
orangeColor = "#ee9a00"
orangeDarkColor = "#9e4a00"
orangeDarkestColor = "#7e2a00"
blueColor = "#2c3c3c"
blackColor = "#222222"
blackDarkColor = "#080808"
whiteColor = "#9999ff"
purpleColor = "#404051"

-- fonts
terminusFont = "-*-terminus-medium-*-*-*-14-*-*-*-*-*-iso10646-*"
