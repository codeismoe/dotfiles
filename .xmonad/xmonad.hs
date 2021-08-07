{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import System.Taffybar.Support.PagerHints (pagerHints)
import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Decoration
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Types

main :: IO ()
main = xmonad' myConfig

xmonad' = xmonad . ewmh . myNavigation . pagerHints . docks . myDecoration

myNavigation = navigation2D def (xK_k, xK_h, xK_j, xK_l) [(mod4Mask, windowGo)] False

data NoTextShrinker = NoText deriving (Show, Read)

instance Shrinker NoTextShrinker where
  shrinkIt _ = const []

myDecoration config = config {layoutHook = decoration NoText theme (SideDecoration U) $ layoutHook config}
  where
    theme =
      def
        { activeBorderWidth = 0,
          inactiveBorderWidth = 0,
          urgentBorderWidth = 0,
          decoHeight = 4
        }
         

myConfig =
  def
    { terminal = "kitty",
      modMask = mod4Mask,
      layoutHook = myLayout,
      borderWidth = 0
    }
    `additionalKeysP` keys
  where
    keys =
      [ ("M-p", spawn "rofi -show run"),
        ("M-S-h", sendMessage $ ExpandTowards L),
        ("M-S-l", sendMessage $ ShrinkFrom L),
        ("M-S-k", sendMessage $ ExpandTowards U),
        ("M-S-j", sendMessage $ ShrinkFrom U),
        ("M-S-M1-h", sendMessage $ ShrinkFrom R),
        ("M-S-M1-l", sendMessage $ ExpandTowards R),
        ("M-S-M1-k", sendMessage $ ShrinkFrom D),
        ("M-S-M1-j", sendMessage $ ExpandTowards D),
        ("M-s", sendMessage Swap),
        ("M-S-s", sendMessage Rotate),
        ("M-M1-j", sendMessage $ SplitShift Prev),
        ("M-M1-k", sendMessage $ SplitShift Next)
      ]
    myLayout = spacingRaw True Border { top = 0, left = 0, right = 0, bottom = 0} True Border {top = 3, left = 3, right = 3, bottom = 3} True $ avoidStruts emptyBSP

newtype SideDecoration a = SideDecoration Direction2D
  deriving (Show, Read)

instance Eq a => DecorationStyle SideDecoration a where
  shrink b (Rectangle _ _ dw dh) (Rectangle x y w h) = case b of
    SideDecoration U -> Rectangle x (y + fi dh) w (h - dh)
    SideDecoration R -> Rectangle x y (w - dw) h
    SideDecoration D -> Rectangle x y w (h - dh)
    SideDecoration L -> Rectangle (x + fi dw) y (w - dw) h

  pureDecoration b dw dh _ st _ (win, Rectangle x y w h)
    | win `elem` W.integrate st && dw < w && dh < h = Just $ case b of
      SideDecoration U -> Rectangle x y w dh
      SideDecoration R -> Rectangle (x + fi (w - dw)) y dw h
      SideDecoration D -> Rectangle x (y + fi (h - dh)) w dh
      SideDecoration L -> Rectangle x y dw h
    | otherwise = Nothing
