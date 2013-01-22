import Data.Monoid
import System.Exit
import System.IO

import XMonad
import XMonad.Actions.CycleWindows -- alt-tab
import XMonad.Actions.MouseGestures
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile

import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
import qualified Data.Map        as M


main = do
  xmobar <- spawnPipe "xmobar"
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { terminal = terminal'
    , modMask = modMask'
    , workspaces = workspaces'
    , borderWidth = borderWidth'
    , normalBorderColor = normalBorderColor'
    , focusedBorderColor = focusedBorderColor'
    , keys = keys'
    , logHook = logHook' xmobar
    , layoutHook = layoutHook'
    , manageHook = manageHook'
    }

-----------------------------------------------------------------------------------
-- hooks

manageHook' :: ManageHook
manageHook' = (doF W.swapDown) <+> manageHook defaultConfig <+> manageDocks

logHook' :: Handle -> X ()
logHook' h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }

layoutHook' = customLayout

-----------------------------------------------------------------------------------
-- looks
-- bar
customPP :: PP
customPP = defaultPP { ppCurrent = xmobarColor "#C70404" ""
                     , ppVisible = xmobarColor "#f8f8f8" "" . wrap "-" "-"
                     , ppTitle = xmobarColor "#747474" "" . shorten 40
                     , ppLayout = xmobarColor "#747474" ""
                     , ppSep = "<fc=#C70404> | </fc>"
                     , ppHiddenNoWindows = xmobarColor "#747474" ""
                     , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "[" "]"
                     }

borderWidth' :: Dimension
borderWidth' = 2

normalBorderColor', focusedBorderColor' :: String
normalBorderColor' = "#222222"
focusedBorderColor' = "#C70404"

workspaces' :: [WorkspaceId]
workspaces' = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

customLayout = avoidStruts $ smartBorders tiled ||| smartBorders (Mirror tiled) ||| noBorders Full
  where
    tiled = ResizableTall 1 (2/100) (3/5) []

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
    , ((modm,               xK_p     ), spawn "dmenu_run -b -nb black -nf white -sb black -sf \"#C70404\"")
    , ((modm .|. shiftMask, xK_c     ), kill)

    -- shortcuts
    , ((modm, xK_f), spawn "firefox")
    , ((modm, xK_s), spawn "spotify")

    -- layouts
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
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
