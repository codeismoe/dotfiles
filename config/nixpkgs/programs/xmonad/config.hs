import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.DynamicProjects

main :: IO ()
main = do
  xmonad $ emwh $ desktopConfig 
    { terminal = "alacritty"
    , modMask = mod4Mask
    , borderWidth = 3
    }
 
