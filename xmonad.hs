import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.ResizableTile

import Graphics.X11.ExtraTypes.XF86  
import Graphics.X11 (noModMask, shiftMask)

import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

    -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    -- Lock screen
    --, ((modm,               xK_s     ), spawn "xscreensaver-command -lock")

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area verticaly
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area verticaly
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Expand the master area horizontal
    , ((modm,               xK_u     ), sendMessage MirrorExpand)

    -- Shrink the master area horizontal
    , ((modm,               xK_d     ), sendMessage MirrorShrink)
 
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    --
    -- Media keys, special bindings
    ++ 
    [ ((noModMask, xF86XK_MonBrightnessUp),
        spawn "light -A 10")
    , ((noModMask, xF86XK_MonBrightnessDown),
        spawn "light -U 10")
    , ((noModMask, xF86XK_AudioMute),
        spawn "amixer -q sset Master toggle")
    , ((noModMask, xF86XK_AudioLowerVolume),
        spawn "amixer -q sset Master 3%-")
    , ((noModMask, xF86XK_AudioRaiseVolume),
        spawn "amixer -q sset Master 3%+")
    ] 

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#002b36"
myFocusedBorderColor = "#dc322f"

------------------------------------------------------------------------
baseConfig = desktopConfig

main = xmonad =<< xmobar baseConfig
    { terminal          = "termite"
    , modMask           = mod4Mask
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , keys              = myKeys
    , focusFollowsMouse = False
    , clickJustFocuses  = False
    -- , startupHook       = do spawn "xcompmgr"
    }
