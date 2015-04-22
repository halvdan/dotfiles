import Data.Monoid
import System.Exit
import System.IO

import XMonad
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseGestures
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionCustom)
import XMonad.Util.NamedScratchpad
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed

import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
import qualified Data.Map        as M


main = do
  statusBar <- spawnPipe statusBar'
  statusBar2 <- spawnPipe statusBar2'
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
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
manageScratchPad = scratchpadManageHook (W.RationalRect (1/3) (1/3) (1/3) (1/3))
scratchPad = scratchpadSpawnActionCustom "urxvt -name scratchpad"

manageHook' :: ManageHook
manageHook' = (doF W.swapDown) <+> manageHook defaultConfig <+> manageDocks <+> manageScratchPad <+> manageFloats

logHook' :: Handle -> X ()
logHook' h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }

layoutHook' = customLayout

startupHook' = setWMName "LG3D"

manageFloats = composeAll $ concat
  [[ title =? t --> doFloat | t <- floatByTitle ]
  ,[ className =? c --> doFloat | c <- floatByClass ]
  ]

floatByTitle = [ "first", "Drop", "asd" ]
floatByClass = [ "gimp", "MyChain" ]

-----------------------------------------------------------------------------------
-- looks

statusBar' = "dzen2 -y '0' -w '811' -ta 'l'" ++ dzenStyle
dzenStyle  = " -h '16' -fg '#888888' -bg '#151515' -fn 'Montecarlo-10'"

statusBar2' = "bash /home/dan/dotfiles/xmonad/rg/statusbar.sh"

customPP :: PP
customPP = defaultPP { ppCurrent = dzenColor "#dddddd" ""
                     , ppVisible = dzenColor "#888888" "" . wrap "-" "-"
                     , ppTitle = dzenColor "#747474" "" . shorten 120
                     , ppLayout = dzenColor "#747474" ""
                     , ppSep = " | "
                     , ppUrgent = dzenColor "#FFFFAF" "" . wrap "[" "]"
                     }

borderWidth' :: Dimension
borderWidth' = 1

normalBorderColor', focusedBorderColor' :: String
normalBorderColor' = "#000000"
focusedBorderColor' = "#00ff00"

workspaces' :: [WorkspaceId]
workspaces' = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

-----------------------------------------------------------------------------------
-- Layouts 
tiled = renamed [Replace "T" ] $ smartBorders $ ResizableTall 1 0.03 0.618 []
mtiled = renamed [Replace "MT"] $ smartBorders $ Mirror tiled
full = renamed [Replace "F"] $ noBorders Full

customLayout = avoidStruts $ tiled ||| mtiled ||| full

-----------------------------------------------------------------------------------
-- Terminal

terminal' :: String
terminal' = "urxvt"

-----------------------------------------------------------------------------------
-- Keys/button bindings

modMask' :: KeyMask
modMask' = mod4Mask

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launching + killing
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), spawn "dmenu_run")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm .|. shiftMask, xK_s     ), scratchPad)

    -- screens
    , ((modm, 	    	    xK_a     ), onPrevNeighbour W.view)
    , ((modm, 	    	    xK_d     ), onNextNeighbour W.view)
    , ((modm .|. shiftMask, xK_a     ), onPrevNeighbour W.shift)
    , ((modm .|. shiftMask, xK_d     ), onNextNeighbour W.shift)

    -- shortcuts
    , ((modm, xK_s), spawn "spotify")
    , ((modm, xK_F9), spawn "setxkbmap -layout se")
    , ((modm, xK_F10), spawn "setxkbmap -layout us")

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
