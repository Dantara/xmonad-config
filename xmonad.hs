import           XMonad

import           XMonad.Actions.CycleWS
import           XMonad.Actions.Navigation2D

import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.FadeWindows
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName

import           XMonad.Layout
import           XMonad.Layout.BorderResize
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Grid
import           XMonad.Layout.Spacing
import           XMonad.Layout.WindowArranger

import           XMonad.Actions.PhysicalScreens
import qualified XMonad.StackSet                as W
import           XMonad.Util.EZConfig


startup :: X ()
startup = do
    spawn "killall polybar feh plank picom conky libinput-gestures"

    -- composite manager
    spawn "sleep 0.1 && picom"

    -- image wallpaper
    spawn "sleep 0.1 && feh --bg-scale ~/Images/Wallpapers/paints_solarized_70.jpg"

    -- bar
    spawn "sleep 0.1 && polybar xmonad"
    spawn "sleep 0.1 && plank"

    spawn "sleep 0.1 && conky"

    spawn "sleep 0.1 && libinput-gestures"
    -- set WM name
    setWMName "LG3D"

myLogHook :: X ()
myLogHook = fadeWindowsLogHook $ composeAll [opaque
                                            , isUnfocused --> transparency 0.125
                                            , (appName =? "emacs") --> transparency 0.05
                                            ]

myWorkspaces :: [String]
myWorkspaces = show <$> [1..9] <> [0]

myLayout = borderResize $ windowArrange $ avoidStruts $
  spacingRaw True (Border 6 6 6 6) True (Border 6 6 6 6) True $
  Tall 1 (3/100) (1/2)
  ||| Grid
  ||| Full

myKeymap :: [(String, X ())]
myKeymap = [("M-r", spawn "rofi -show run")
           , ("M-o", spawn "rofi -show drun")
           , ("M-w", spawn "rofi -show window")
           , ("M-\\", spawn "rofi-hoogle")
           , ("M-C-S-l", spawn "dm-tool lock")
           , ("M-<L>", prevWS)
           , ("M-<R>", nextWS)
           , ("M-S-<L>", shiftToPrev)
           , ("M-S-<R>", shiftToNext)
           , ("M-`", toggleWS)
           , ("M-C-h", sendMessage Shrink)
           , ("M-C-l", sendMessage Expand)
           , ("M-b", sendMessage ToggleStruts)
           , ("<Print>", spawn "scrot ~/Images/Screenshots/%Y-%m-%d-%T-screenshot.png")
           , ("M-<Print>", spawn "scrot ~/Images/Screenshots/%Y-%m-%d-%T-screenshot.png")
           ] <>
           [("M-" <> mask <> key, f n)
           | (key, n) <- zip ["a", "q"] [1, 0]
           , (mask, f) <- [("", viewScreen def), ("S-", sendToScreen def)]
           ] <>
           [ ("M-" <> mask <> key, f tag)
           | (tag, key) <- zip myWorkspaces (show <$> [1..9] <> [0])
           , (mask, f) <- [("", windows . W.view), ("S-", windows . W.shift)]]

myFnKeys =
  -- backlight
  [ ("<XF86MonBrightnessUp>"   , spawn "xbacklight -inc 10")
  , ("<XF86MonBrightnessDown>" , spawn "xbacklight -dec 10")
  -- volume
  , ("<XF86AudioRaiseVolume>"  , spawn "pactl set-sink-volume 0 +5%")
  , ("<XF86AudioLowerVolume>"  , spawn "pactl set-sink-volume 0 -5%")
  , ("<XF86AudioMute>"         , spawn "pactl set-sink-mute 0 toggle")
  ]

myManageHook :: ManageHook
myManageHook = composeAll [manageDocks
               , fullscreenManageHook
               , appName =? "chromium" --> doShift "1"
               , appName =? "emacs" --> doShift "2"
               , className =? "TelegramDesktop" --> doShift "4"
               ]
main :: IO ()
main =
  xmonad . ewmh . docks . navigation . fullscreenSupport $ def
    { terminal    = "urxvt"
    , modMask     = mod4Mask
    , startupHook = startup
    , workspaces = myWorkspaces
    , borderWidth = 0
    , layoutHook = myLayout
    , logHook = myLogHook
    , manageHook = myManageHook
    }
    `additionalKeysP` (myKeymap <> myFnKeys)
  where
    navigation = navigation2DP def ("k", "h", "j", "l") [("M-", windowGo), ("M-S-", windowSwap)] False
