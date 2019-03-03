import Data.Monoid
import System.Exit
import System.IO

import Graphics.X11.ExtraTypes.XF86

import XMonad
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseGestures
import XMonad.Actions.PhysicalScreens
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Hooks.InsertPosition
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionCustom)
import XMonad.Util.NamedScratchpad
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed

import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
import qualified Data.Map        as M


main = do
  statusBar <- spawnPipe statusBar'
  xmonad $ withUrgencyHook NoUrgencyHook $ desktopConfig
    { terminal = terminal'
    , modMask = modMask'
    , workspaces = workspaces'
    , borderWidth = borderWidth'
    , normalBorderColor = normalBorderColor'
    , focusedBorderColor = focusedBorderColor'
    , keys = keys'
    , logHook = logHook' statusBar
    , layoutHook = layoutHook'
    , manageHook = manageHook'
    , startupHook = startupHook'
    }

-----------------------------------------------------------------------------------
-- hooks

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect (1/6) (1/6) (2/3) (2/3))
scratchPad = scratchpadSpawnActionCustom "termite --name=scratchpad"

manageHook' :: ManageHook
manageHook' = (doF W.swapDown) <+> manageHook desktopConfig <+> manageDocks <+> manageScratchPad <+> manageFloats

logHook' :: Handle -> X ()
logHook' h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }

layoutHook' = customLayout

startupHook' = setWMName "LG3D"

manageFloats = composeAll [ isDialog --> doCenterFloat'
                          , isFullscreen --> doFullFloat
                          , className =? "MPlayer" --> doFloat
                          , className =? "mplayer2" --> doFloat
                          , className =? "Gimp" --> doFloat
                          , className =? "dev" --> doFloat
                          , className =? "MyGdxGame" --> doFloat
                          , insertPosition Below Newer
                          ]
  where
    doMaster = doF W.shiftMaster
    doCenterFloat' = doCenterFloat <+> doMaster

-----------------------------------------------------------------------------------
-- looks

statusBar' = "xmobar ~/.xmonad/xmobarrc"
--statusBar' = "dzen2 -xs 1 -y '0' -w '800' -ta 'l'" ++ dzenStyle
--dzenStyle  = " -h '16' -fg '#888888' -bg '#151515' -fn 'Montecarlo-10'"

-- statusBar2' = "bash /home/dan/dotfiles/xmonad/abyss/statusbar.sh"

customPP :: PP
customPP = defaultPP { ppCurrent = xmobarColor "#dddddd" ""
                     , ppVisible = xmobarColor "#888888" "" . wrap "-" "-"
                     , ppTitle = xmobarColor "#747474" ""
                     , ppLayout = xmobarColor "#747474" ""
                     , ppSep = xmobarColor "#444444" "" " | "
                     , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "[" "]"
                     }

borderWidth' :: Dimension
borderWidth' = 2

gapWidth' = 10

normalBorderColor', focusedBorderColor' :: String
normalBorderColor' = "#1c1c1c"
focusedBorderColor' = "#444444"

workspaces' :: [WorkspaceId]
workspaces' = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

-----------------------------------------------------------------------------------
-- Layouts 
tiled = renamed [Replace "[]=" ] $ smartSpacing gapWidth' $ smartBorders $ ResizableTall 1 (2/100) (1/2) []
mtiled = renamed [Replace "=[]"] $ smartSpacing gapWidth' $ smartBorders $ Mirror tiled
full = renamed [Replace "[]"] $ noBorders Full
tab = renamed [Replace "T"] $ noBorders $ tabbed shrinkText tabTheme1

tabTheme1 = defaultTheme { decoHeight = 16
                         , activeColor = "#a6c292"
                         , activeBorderColor = "#a6c292"
                         , activeTextColor = "#000000"
                         , inactiveBorderColor = "#000000"
                         }

customLayout = avoidStruts $ tiled ||| mtiled ||| tab ||| full

-----------------------------------------------------------------------------------
-- Terminal

terminal' :: String
terminal' = "termite"

-----------------------------------------------------------------------------------
-- Keys/button bindings

modMask' :: KeyMask
modMask' = mod4Mask

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launching + killing
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), spawn "dmenu_run")
    , ((modm,               xK_o     ), spawn "xcmenuctrl")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm .|. shiftMask, xK_s     ), scratchPad)

    -- screens
    , ((modm, 	    	    xK_a     ), onPrevNeighbour W.view)
    , ((modm, 	    	    xK_d     ), onNextNeighbour W.view)
    , ((modm .|. shiftMask, xK_a     ), onPrevNeighbour W.shift)
    , ((modm .|. shiftMask, xK_d     ), onNextNeighbour W.shift)

    -- shortcuts
    , ((modm, xK_f), spawn "firefox")
    , ((modm, xK_s), spawn "spotify")

    -- spotify controls
    --, ((0, xF86XK_AudioPlay), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
    --, ((0, xF86XK_AudioNext), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
    --, ((0, xF86XK_AudioPrev), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
    , ((0, xF86XK_AudioMicMute), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")

    , ((modm, xK_F11), spawn "setxkbmap -layout se")
    , ((modm, xK_F12), spawn "setxkbmap -layout us")

    -- layouts
    , ((modm,               xK_space ), toggleWS' ["NSP"])
    , ((modm,               xK_e     ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_e     ), setLayout $ XMonad.layoutHook conf)
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- floating layer
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- refresh
    , ((modm,               xK_n     ), refresh)

    -- focus
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_Left  ), windows W.focusUp)
    , ((modm,               xK_Right ), windows W.focusDown)
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- swapping
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- number of windows
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_n     ), sendMessage MirrorShrink)
    , ((modm,               xK_i     ), sendMessage MirrorExpand)

    -- quitting + restart
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [ ((m .|. modm, k), windows $ f i) 
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]


mouseBindings' (XConfig { XMonad.modMask = modMask }) = M.fromList $
  [ ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) ]
