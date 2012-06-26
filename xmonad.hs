-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import Dzen

import XMonad
import XMonad.ManageHook
import XMonad.Hooks.DynamicLog hiding (dzen)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import XMonad.Layout.Named

-- Special keys
import XMonad.Util.EZConfig
import Graphics.X11.ExtraTypes.XF86  

import Data.Monoid
import System.Exit
import System.IO (hPutStrLn)


import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal          = "xterm"
myTerminalStartup   = " -e \"archey -c green;bash;\"" 

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod1Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1:web","2:dev","3:chat", "4:mp3"] ++ 
                  map show [5..8] ++
                  ["9:msg"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#990000"

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
    , ((modm,               xK_s     ), spawn "xscreensaver-command -lock")

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Change layout to fullscreen
    , ((modm,               xK_b     ), sendMessage $ JumpToLayout "full")

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
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "killall conky dzen2; xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    --
    -- Media keys, special bindings
    ++ 
    [ ((modm, xF86XK_AudioNext), spawn "/home/thall/scripts/spotify_next.sh")
    , ((modm, xF86XK_AudioPrev), spawn "/home/thall/scripts/spotify_prev.sh")
    ] 
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
-- Horizontal split
-- named "Horizontal" (smartBorders $ Mirror resizeTiled)

myLayout = avoidStruts (
                named "default" (smartBorders resizeTiled) 
            ||| named "three" (smartBorders threeCol) 
            ||| named "borderFull" (smartBorders Full)
           )||| named "full" (noBorders Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     
     threeCol    = ThreeCol nmaster delta ratio

     tiled   = Tall nmaster delta ratio
     resizeTiled = ResizableTall nmaster delta ratio []

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: ManageHook
myManageHook = (composeAll . concat $
  [ [ resource  =? r   --> doIgnore | r <- myIgnores] -- my ignores
  , [ className =? c   --> doShift "1:web"  | c <- myWebs]
  , [ className =? c   --> doShift "2:dev"  | c <- myDevs]
  , [ className =? c   --> doShift "3:chat" | c <- myChat]
  , [ className =? c   --> doShift "4:mp3"  | c <- myMp3s]
  , [ className =? c   --> doCenterFloat    | c <- myFloats]
  ])

  where
  myIgnores = ["desktop_window", "kdesktop"]
  myWebs    = ["Google-chrome", "Chromium", "Firefox"]
  myDevs    = ["Threadscope"]
  myChat    = ["Pidgin", "Mangler"]
  myMp3s    = ["Spotify"]
  myFloats  = [] --myChat

{-
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]
-}
------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook h = dynamicLogWithPP  $ defaultPP
  {  ppCurrent          = dzenColor "#303030" "#909090" . pad
  ,  ppHidden           = dzenColor "#909090" "" . pad
  ,  ppHiddenNoWindows  = dzenColor "606060" "" . pad
  ,  ppLayout           = dzenColor "#909090" "" . pad
  ,  ppUrgent           = dzenColor "#ff0000" "" . pad . dzenStrip
  ,  ppTitle            = shorten 100 
  ,  ppWsSep            = ""
  ,  ppSep              = "  "
  ,  ppOutput           = hPutStrLn h  
  }  

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()


-- Bars
myLeftBar :: DzenConf
myLeftBar = defaultDzen
  { width   = Just $ Percent 55
  , fgColor = Just "#909090"
  , bgColor = Just "#303030"
  }

myRightBar :: DzenConf
myRightBar = myLeftBar 
  { xPosition = Just $ Percent 55
  , width     = Just $ Percent 45
  , alignment = Just RightAlign
  }
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  d <- spawnDzen myLeftBar

  spawnToDzen "conky -c ~/.xmonad/data/conky/dzen" myRightBar

  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
      -- simple stuff
        terminal           = myTerminal ++ myTerminalStartup,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook d,
        startupHook        = myStartupHook
    }
