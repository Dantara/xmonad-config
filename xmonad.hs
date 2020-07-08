import           XMonad
import           XMonad.Config.Desktop
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout
import           XMonad.Layout.Gaps
import           XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Hooks.SetWMName
import XMonad.Hooks.FadeInactive
import XMonad.Layout.Fullscreen
import XMonad.Hooks.FadeWindows


startup :: X ()
startup = do
    spawn "killall polybar feh plank picom"

    -- composite manager
    spawn "sleep 0.1 && picom -CG"

    -- image wallpaper
    spawn "sleep 0.1 && feh --bg-scale ~/Images/Wallpapers/mountain-cropped.jpg"

    -- bar
    spawn "sleep 0.3 && polybar xmonad"
    spawn "sleep 0.1 && plank"
 
    -- set WM name
    setWMName "LG3D"

myLogHook :: X ()
myLogHook = fadeWindowsLogHook $ composeAll [isUnfocused --> transparency 0.15
                                            , (appName =? "chromium") --> opaque
                                            , (className =? "Gimp-2.10") --> opaque
                                            ]
         
myWorkspaces :: [String]
myWorkspaces = show <$> [1..9]

gap :: Int
gap = 0

fi :: Integer -> Integer
fi = fromIntegral

myLayout = avoidStruts $ spacingRaw True (Border 6 6 6 6) True (Border 6 6 6 6) True $
  Tall 1 (3/100) (1/2)
  ||| Grid
  ||| fullscreenFull Full

main :: IO ()
main =
  xmonad . ewmh . docks . fullscreenSupport $ def
    { terminal    = "urxvt"
    , modMask     = mod4Mask
    , startupHook = startup
    , workspaces = myWorkspaces
    , borderWidth = 0
    , layoutHook = myLayout
    , logHook = myLogHook
    , manageHook = manageDocks <> fullscreenManageHook
    }
