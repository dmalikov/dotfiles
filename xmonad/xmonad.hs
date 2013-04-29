{-# LANGUAGE UnicodeSyntax #-}
import           Control.Applicative             ((<$>))
import           Control.Monad                   (liftM2)
import           Data.List                       (isPrefixOf)
import           Data.Map                        (fromList, union)
import           Data.Maybe                      (isJust)
import           Data.Time                       (getCurrentTime)
import           Data.Time.Format                (formatTime)
import           System.Directory                (getHomeDirectory, renameFile)
import           System.FilePath.Posix           (joinPath, (<.>))
import           System.IO                       (Handle, hPutStrLn)
import           System.Locale                   (defaultTimeLocale)

import           XMonad
import qualified XMonad.Actions.CycleWS          as CWS
import           XMonad.Actions.SpawnOn          (spawnOn)
import           XMonad.Actions.SwapWorkspaces   (swapWithCurrent)
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Combo
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.Tabbed
import           XMonad.Layout.TwoPane
import           XMonad.Layout.WindowNavigation
import           XMonad.Prompt
import           XMonad.Prompt.Input             (inputPromptWithCompl, (?+))
import           XMonad.Prompt.Shell
import qualified XMonad.StackSet                 as S
import           XMonad.Util.Run                 (runProcessWithInput,
                                                  spawnPipe)
import           XMonad.Util.Scratchpad
import           XMonad.Util.WorkspaceScreenshot

main ∷ IO ()
main = do
  initCapturing
  xmproc <- spawnPipe xmobar_run
  xmonad $ defaultConfig
    { borderWidth        = 1
    , focusFollowsMouse  = False
    , focusedBorderColor = orangeDarkestColor
    , keys               = liftM2 union myKeys (keys defaultConfig)
    , layoutHook         = myLayoutHook
    , logHook            = myLogHook xmproc
    , manageHook         = manageDocks <+> manageHook defaultConfig <+> myManageHook
    , modMask            = mod4Mask
    , normalBorderColor  = blackDarkColor
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
  , ( ( modm .|. controlMask, xK_u      ), spawn uzbl_tabbed )
  , ( ( modm .|. controlMask, xK_j      ), spawnOn "t" jws_irssi )
  , ( ( modm .|. controlMask, xK_p      ), spawn mpc_toggle )
  , ( ( modm .|. controlMask, xK_period ), spawn mpc_next )
  , ( ( modm                , xK_j      ), CWS.prevWS )
  , ( ( modm                , xK_k      ), CWS.nextWS )
  , ( ( modm                , xK_s      ), scratchpadSpawnAction conf )
  , ( ( modm                , xK_g      ), spawn gvim )
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

myLogHook ∷ Handle → X ()
myLogHook h = do
  ewmhDesktopsLogHook
  updatePointer (Relative 0.5 0.5)
  dynamicLogWithPP $ xmobarPP
    { ppCurrent = xmobarColor orangeColor ""
    , ppLayout  = const ""
    , ppOutput  = hPutStrLn h
    , ppSep     = " | "
    , ppSort    = fmap (. scratchpadFilterOutWorkspace) $ ppSort xmobarPP
    , ppTitle   = xmobarColor orangeDarkColor ""
    }
  fadeInactiveLogHook 0.7

myManageHook ∷ ManageHook
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
      , className =? "stalonetray"
      ]

scratchpadHook = scratchpadManageHook (S.RationalRect paddingLeft paddingTop width height')
  where
    height'     = 0.9
    width       = 0.9
    paddingTop  = 0.05
    paddingLeft = (1 - width) / 2

myLayoutHook = smartBorders . avoidStruts $
  resizableTile ||| myTwoPaneLeft ||| myTwoPaneRight ||| myTabbed
  where
    resizableTile  = ResizableTall 1 (1/30) (1/2) []
    myTwoPaneLeft  = windowNavigation $ combineTwo (TwoPane (1/10) (1/3)) (tabbedAlways shrinkText myTheme) Full
    myTwoPaneRight = windowNavigation $ combineTwo (TwoPane (1/10) (2/3)) Full (tabbedAlways shrinkText myTheme)
    myTabbed       = tabbed shrinkText myTheme

myXPConfig ∷ XPConfig
myXPConfig = defaultXPConfig
  { font = terminusFont
  , bgColor = blackDarkColor
  , borderColor = blackColor
  , fgColor = blueLightColor
  , position = Bottom
  , height = 28
  , autoComplete = Nothing
  }

myTheme = defaultTheme
  { activeColor = blackDarkColor
  , activeBorderColor = blackDarkColor
  , activeTextColor = orangeColor
  , inactiveColor = blackDarkColor
  , inactiveBorderColor = blackDarkColor
  , inactiveTextColor = orangeDarkestColor
  , fontName = terminusFont
  , decoHeight = 22
}

tmuxRun ∷ IO [String]
tmuxRun = lines <$> runProcessWithInput "tmux" ["list-sessions", "-F", "#{session_name}"] ""

tmuxPrompt ∷ XPConfig → X ()
tmuxPrompt c = io tmuxRun >>= \as -> inputPromptWithCompl c "tmux" (mkComplFunFromList' as) ?+ tmuxStart as

tmuxStart ∷ [String] → String → X ()
tmuxStart ss s = asks (terminal . config) >>= \term -> if s `elem` ss then attach term s else create term s
 where
  attach = \t s' -> spawn $ t ++ " -e tmux attach -t " ++ s'
  create = \t s' -> spawn $ t ++ " -e tmux new -s " ++ s'


-- Sets the WM name to a given string, so that it could be
-- detected using _NET_SUPPORTING_WM_CHECK protocol
myStartupHook ∷ X ()
myStartupHook = setWMName "LG3D"

myTerminal = urxvt ++ " -cd ~/"

myWorkspaces  = [ "♫", "τ", "Λ" ] ++ map show [4..9] ++ [ "α", "β", "λ", "ρ", "«", "»" ]

-- applications
gvim = "gvim"
hpasteit = "xclip -o -selection primary | hpasteit | xclip -i -selection primary"
jws_irssi = urxvt ++ " -title jεωs -e ssh jws -t 'export LANG=en_US.UTF-8; tmux attach-session -t irssi'"
lock_screen = "xscreensaver-command -lock"
mpc_next = "mpc next"
mpc_toggle = "mpc toggle"
urxvt = "urxvt -name shiva"
uzbl_tabbed = "uzbl-tabbed"
volume_decrease = "amixer sset 'Master',0 5%-"
volume_increase = "amixer sset 'Master',0 5%+"
volume_toggle_mute = "amixer set Master toggle"
xmobar_run = "~/.xmonad/xmobar"

-- colors
orangeColor = "#ee9a00"
orangeDarkColor = "#9e4a00"
orangeDarkestColor = "#7e2a00"
blueColor = "#2c3c3c"
blueLightColor = "#22cc99"
blackColor = "#222222"
blackDarkColor = "#080808"
whiteColor = "#9999ff"
purpleColor = "#404051"

-- fonts
terminusFont = "-*-terminus-medium-*-*-*-14-*-*-*-*-*-iso10646-*"
