{-# LANGUAGE OverloadedStrings #-}
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.DynamicProjects
import XMonad.Util.EZConfig
import Graphics.X11.Types

appLauncher  = "rofi -modi drun,ssh,window -show drun -show-icons"

main :: IO ()
main = do
  xmonad $ ewmh $ desktopConfig
    { terminal = "alacritty"
    , modMask = mod4Mask
    , borderWidth = 3
    , normalBorderColor = "#2e2e2e"
    , focusedBorderColor = "#d6d6d6"
    } `additionalKeys`
    [ ((mod4Mask, xK_p), spawn appLauncher)
    ] `additionalKeysP`
    [ ("<XF86MonBrightnessUp>", spawn "light -A 5")
    , ("<XF86MonBrightnessDown>", spawn "light -U 5")
    ]
