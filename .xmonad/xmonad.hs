import XMonad
import XMonad.Operations
import System.IO
import System.Exit
import XMonad.Util.Run
import Graphics.X11.ExtraTypes.XF86
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import Data.Monoid
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import System.Exit
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Fullscreen
import XMonad.Util.SpawnOnce

-- hjkl navigation
import XMonad.Layout.WindowNavigation

-- Has isFullscreen, for games/ other fullscreen stuff.
--import Xmonad.Hooks.EwmhDesktops

main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"

myPP = xmobarPP { ppCurrent = xmobarColor "#bf616a" ""
                , ppHidden = xmobarColor "#c0c5ce" ""
                , ppHiddenNoWindows = xmobarColor "#4f5b66" ""
                , ppUrgent = xmobarColor "#a3be8c" ""
                , ppLayout = xmobarColor "#4f5b66" ""
                , ppTitle =  xmobarColor "#c0c5ce" "" . shorten 80
                , ppSep = xmobarColor "#4f5b66" "" "  "
                }


-- Key binding to toggle the gap for the 'top' bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_t)

-- myTerminal = "urxvt"
myTerminal = "urxvt"

-- Main configuration, override the defaults to your liking.
myConfig = def { modMask = mod3Mask
               , terminal = myTerminal
               , workspaces = myWorkspaces
               , keys = myKeys
               , layoutHook = smartBorders $ myLayoutHook
               --, focusedBorderColor = "#2E9AFE"4f5b66
               , focusedBorderColor = "#4f5b66"
               , normalBorderColor = "#000000"
               , manageHook = myManageHook <+> manageHook def
               , mouseBindings = myMouseBindings
               , borderWidth = 2
               , startupHook = myStartupHook
               }


--   icons:       Chrome    terminal   code     folders   twitter   spotify   games
--myWorkspaces = ["\xf268", "\xf120", "\xf121", "\xf07b", "\xf099", "\xf1bc", "\xf11b"]

--   icons:                     Chrome   terminal  code    folders   reddit   video       twitter  youtube  reddit
myWorkspaces = map wrapSpaces ["\xf269","\xf120","\xf121","\xf07c","\xf281","\xf04e"] --,"\xf099","\xf16a","\xf281"]
    where wrapSpaces s = "" ++ s ++ ""

--xmobarEscape = concatMap doubleLts
--  where doubleLts '<' = "<<"
--        doubleLts x    = [x]
--
--myWorkspaces = clickable . (map xmobarEscape) $ ["\xf269","\xf120","\xf0e0","\xf07c","\xf1b6","\xf281","\xf099" ,"\xf16a","\xf04e"]
--   where
--       clickable l = [ "<action=xdotool key alt+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
--           (i,ws) <- zip [1..9] l,
--                           let n = i ]

-- Separate mask for some keys. Caps for some, alt for others..
altMask = mod1Mask
myKeys conf@(XConfig {XMonad.modMask = capsMask}) = M.fromList $

    -- Launch programs: Terminal, Dmenu, Chrome, Emacs.
    [ ((capsMask, xK_Return), spawn myTerminal)
    , ((capsMask, xK_d), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    --, ((capsMask, xK_b), spawn "google-chrome-stable")
    , ((capsMask, xK_b), spawn "firefox")
    , ((capsMask, xK_e), spawn "emacs")

    , ((altMask, xK_q), kill) -- quit current window

     -- Rotate through the available layout algorithms
    , ((capsMask, xK_space), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((altMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((capsMask, xK_n), refresh)

    -- HJKL moves between windows.
    -- Shift + HJKL swaps windows.
    , ((capsMask,               xK_h), sendMessage $ Go L)
    , ((capsMask,               xK_j), sendMessage $ Go D)
    , ((capsMask,               xK_k), sendMessage $ Go U)
    , ((capsMask,               xK_l), sendMessage $ Go R)

    , ((capsMask .|. altMask, xK_h), sendMessage $ Swap L)
    , ((capsMask .|. altMask, xK_j), sendMessage $ Swap D)
    , ((capsMask .|. altMask, xK_k), sendMessage $ Swap U)
    , ((capsMask .|. altMask, xK_l), sendMessage $ Swap R)

    -- Shrink/expand master area.
    , ((             altMask, xK_h), sendMessage Shrink)
    , ((             altMask, xK_l), sendMessage Expand)

    -- Increment/Decrement the number of windows in the master area
    , ((capsMask, xK_comma),  sendMessage (IncMasterN (-1)))
    , ((capsMask, xK_period), sendMessage (IncMasterN 1))

    -- Push window back into tiling
    , ((capsMask,               xK_t), withFocused $ windows . W.sink)

    -- Volume
    , ((0, xF86XK_AudioMute),        spawn "amixer -D pulse set Master toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer -D pulse set Master unmute 5%-")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -D pulse set Master unmute 5%+")

    -- Screen brightness
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10") -- BROKEN ubuntu 16.04. TODO: Find fix.
    , ((0, xF86XK_MonBrightnessUp),   spawn "xbacklight -inc 10")

    -- Quit xmonad (logout)
    , ((altMask .|. shiftMask, xK_e     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((altMask .|. shiftMask, xK_r     ), spawn "xmonad --recompile; xmonad --restart")
    ]

    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. capsMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, altMask)]]

     ++

     -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
     -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. capsMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_a, xK_o] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, altMask)]]

myStartupHook = do
  --spawnOnce "/usr/bin/stalonetray"
  spawnOnce "nm-applet"
  spawnOnce "volumeicon"
  spawnOnce "dropbox"
  spawnOnce "compton -cb"
  spawnOnce "redshift-gtk"

myManageHook = composeAll
    [ className =? "stalonetray"  --> doIgnore
    , isFullscreen                --> doFullFloat
    , className =? "mpv"          --> doFullFloat
    , manageDocks
    --, className =? "Steam"        --> doFullFloat
    --, title =? "LIMBO"            --> doIgnore
    --, title =? "FEZ"              --> doIgnore
    --, title =? "NMRIH"            --> doFullFloat
    --, title =? "Portal"            --> doFullFloat
    --, className =? "firefox"      --> doFullFloat
    --, isFullscreen                --> (doF W.focusDown <+> doFullFloat)
    ]

-- Mouse bindings

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

-- myLayoutHook = avoidStruts (Grid ||| tiled ||| noBorders (fullscreenFull Full) ||| Mirror tiled)
--myLayoutHook = avoidStruts (tiled ||| simpleTabbed)
myLayoutHook = windowNavigation (tiledWindowLayout ||| fullWindowLayout)

-- Fullscreen
fullWindowLayout = noBorders (fullscreenFull Full)

-- Split screen into two panes. Left = master.
tiledWindowLayout = Tall nmaster delta ratio
  where nmaster = 1     -- 1 window in master pane
        ratio   = 4/7   -- Ratio to split screen.
        delta   = 3/100 -- Percent of screen to increment by when resizing panes
