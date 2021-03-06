-- vim: set ft=haskell:
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative             ((<$>))
import           Control.Monad                   (liftM2)
import           Data.Default
import           Data.List                       (isPrefixOf)
import           Data.Map                        (fromList, union)
import           Data.Maybe                      (isJust)
import           Data.Time                       (getCurrentTime)
import           Data.Time.Format                (formatTime)
import           System.Directory                (getHomeDirectory, renameFile)
import           System.FilePath.Posix           (joinPath, (<.>))
import           System.IO                       (Handle)
import           System.Locale                   (defaultTimeLocale)

import           XMonad
import qualified XMonad.Actions.CycleWS          as CWS
import           XMonad.Actions.SpawnOn          (spawnOn)
import           XMonad.Actions.SwapWorkspaces   (swapWithCurrent)
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Tabbed
import           XMonad.Prompt
import           XMonad.Prompt.Input             (inputPromptWithCompl, (?+))
import           XMonad.Prompt.Shell
import qualified XMonad.StackSet                 as S
import           XMonad.Util.Run                 (runProcessWithInput, spawnPipe)
import           XMonad.Util.Scratchpad
import           XMonad.Util.WorkspaceScreenshot

main :: IO ()
main = do
  initCapturing
  xmproc <- spawnPipe xmobar'
  xmonad $ def
    { borderWidth        = 1
    , focusedBorderColor = orangeDarkestColor
    , normalBorderColor  = blackDarkColor
    , focusFollowsMouse  = False
    , keys               = liftM2 union myKeys (keys def)
    , layoutHook         = myLayoutHook
    , logHook            = myLogHook xmproc
    , manageHook         = manageDocks <+> manageHook def <+> myManageHook
    , modMask            = mod4Mask
    , startupHook        = myStartupHook
    , terminal           = myTerminal
    , workspaces         = myWorkspaces
    }

myKeys conf@(XConfig {modMask = modm}) = fromList $
  [ ( ( modm                , xK_p      ), shellPrompt myXPConfig )
  , ( ( modm .|. shiftMask  , xK_t      ), tmuxPrompt myXPConfig )
  , ( ( modm .|. controlMask, xK_l      ), spawn lock_screen )
  , ( ( modm .|. controlMask, xK_9      ), spawn volume_decrease )
  , ( ( modm .|. controlMask, xK_0      ), spawn volume_increase )
  , ( ( modm .|. controlMask, xK_m      ), spawn volume_toggle_mute )
  , ( ( modm .|. controlMask, xK_j      ), spawnOn "t" do_weechat )
  , ( ( modm .|. controlMask, xK_p      ), spawn mpc_toggle )
  , ( ( modm .|. controlMask, xK_period ), spawn mpc_next )
  , ( ( modm                , xK_j      ), CWS.prevWS )
  , ( ( modm                , xK_k      ), CWS.nextWS )
  , ( ( modm                , xK_s      ), scratchpadSpawnAction conf )
  , ( ( modm                , xK_a      ), sendMessage MirrorShrink )
  , ( ( modm                , xK_z      ), sendMessage MirrorExpand )
  , ( ( modm                , xK_m      ), windows S.swapMaster )
  , ( ( modm .|. shiftMask  , xK_u      ), captureWorkspacesWhen visible moveToImg horizontally )
  , ( ( modm .|. shiftMask  , xK_y      ), captureWorkspacesWhen current moveToImg horizontally )
  , ( ( modm .|. shiftMask  , xK_a      ), captureWorkspacesWhen defaultPredicate moveToImg horizontally )
  , ( ( modm .|. shiftMask  , xK_h      ), spawn hpasteit )
  , ( ( modm                , xK_b      ), CWS.toggleWS' ["NSP"] )
  ]
  ++
  [ ( ( modm .|. controlMask, k         ), withWindowSet swapWindows )
    | (i, k) <- zip myWorkspaces [xK_1..xK_8]
    , let swapWindows s = do
            windows $ swapWithCurrent i
            windows $ S.greedyView $ S.currentTag s
  ]
    where
      visible :: WindowSpace -> X Bool = return . isJust . S.stack
      current :: WindowSpace -> X Bool = \\w -> withWindowSet $ (\\ws -> return $ (S.tag . S.workspace . S.current $ ws) == S.tag w)
      moveToImg :: FilePath -> IO () = \\filepath -> do
        hd <- getHomeDirectory
        date <- formatTime defaultTimeLocale "%F-%X" <$> getCurrentTime
        let newFileName = joinPath [hd, "img", "screen", "own", "xmonad", date] <.> "png"
        renameFile filepath newFileName

myLogHook :: Handle -> X ()
myLogHook h = do
  ewmhDesktopsLogHook
  updatePointer (0.5,0.5) (0,0)
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
      ]
    myIgnores = foldr1 (<||>)
      [ resource =? "panel"
      , resource =? "trayer"
      ]

scratchpadHook = scratchpadManageHook (S.RationalRect paddingLeft paddingTop width height')
  where
    height'     = 0.9
    width       = 0.9
    paddingTop  = 0.05
    paddingLeft = (1 - width) / 2

myLayoutHook = smartBorders . avoidStruts $
  ResizableTall 1 (1/30) (1/2) [] |||
  tabbed shrinkText myTheme

myXPConfig :: XPConfig
myXPConfig = def
  { font = terminusFont
  , bgColor = blackDarkColor
  , borderColor = blackColor
  , fgColor = blueLightColor
  , position = Bottom
  , height = 28
  , autoComplete = Nothing
  }

myTheme = def
  { activeColor = blackDarkColor
  , activeBorderColor = blackDarkColor
  , activeTextColor = orangeColor
  , inactiveColor = blackDarkColor
  , inactiveBorderColor = blackDarkColor
  , inactiveTextColor = orangeDarkestColor
  , fontName = terminusFont
  , decoHeight = 22
}

tmuxRun :: IO [String]
tmuxRun = lines <$> runProcessWithInput "tmux" ["list-sessions", "-F", "#{session_name}"] ""

tmuxPrompt :: XPConfig -> X ()
tmuxPrompt c = io tmuxRun >>= \\as -> inputPromptWithCompl c "tmux" (mkComplFunFromList' as) ?+ tmuxStart as

tmuxStart :: [String] -> String -> X ()
tmuxStart ss s = asks (terminal . config) >>= \\term -> attachOrCreate term s
 where
  attachOrCreate = \\t s' -> spawn $ t ++ " -e tmux new -s " ++ s' ++ " -A"


myStartupHook :: X ()
myStartupHook = do
-- Sets the WM name to a given string, so that it could be
-- detected using _NET_SUPPORTING_WM_CHECK protocol
  setWMName "LG3D"


myTerminal = urxvt ++ " -cd ~/"

myWorkspaces  = [ "♫", "τ", "Λ" ] ++ map show [4..9] ++ [ "α", "β", "λ", "ρ", "«", "»" ]

-- applications
hpasteit = "xclip -o -selection primary | hpasteit | xclip -i -selection primary"
do_weechat = urxvt ++ " -title jεωs -e ssh m@toje.ws -t 'export LANG=en_US.UTF-8 && W=weechat && tmux new -s $W -A'"
lock_screen = "xtrlock"
mpc_next = "mpc next"
mpc_toggle = "mpc toggle"
urxvt = "urxvt -name shiva"
volume_decrease = "amixer sset 'Master',0 5%-"
volume_increase = "amixer sset 'Master',0 5%+"
volume_toggle_mute = "amixer set Master toggle"
xmobar' = "~/.xmonad/xmobar"

-- colors
orangeColor = "#ee9a00"
orangeDarkColor = "#9e4a00"
orangeDarkestColor = "#7e2a00"
blueColor = "#2c3c3c"
blueLightColor = "#22cc99"
blackColor = "#222222"
blackDarkColor = "#1c1c1c"
whiteColor = "#9999ff"
purpleColor = "#404051"

-- fonts
terminusFont = "-*-terminus-medium-*-*-*-12-*-*-*-*-*-iso10646-1"
