{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Config.XMobar (xmobarProp) where

import Graphics.X11.Types (Window)
import XMonad (KeyMask, KeySym, Layout, XConfig (XConfig, modMask), xK_b)
import XMonad.Core (LayoutClass)
import XMonad.Hooks.ManageDocks (AvoidStruts)
import XMonad.Hooks.StatusBar (statusBarProp, withEasySB)
import XMonad.Hooks.StatusBar.PP (PP (ppCurrent, ppHidden, ppSep, ppTitle, ppTitleSanitize, ppVisible, ppWsSep), def, filterOutWsPP, shorten, wrap, xmobarBorder, xmobarColor, xmobarStrip)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)

xmobarProp :: (LayoutClass l Window) => XConfig l -> XConfig (ModifiedLayout AvoidStruts l)
xmobarProp = withEasySB (statusBarProp "xmobar-configured -x 0 ~/.config/xmobar/xmobar.hs" (pure (filterOutWsPP [scratchpadWorkspaceTag] myXmobarPP))) toggleStrutsKey
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig {modMask = m} = (m, xK_b)

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = grey "  \xf01d9  ",
      ppCurrent = blue . wrap "" "" . xmobarBorder "Top" "#89b4fa" 3,
      -- , ppCurrent = blue
      ppHidden = grey,
      ppVisible = white,
      ppWsSep = "  ",
      ppTitleSanitize = xmobarStrip . shorten 30, -- `shorten` defines the max length
      ppTitle = wrap "\xf0570 " ""
      -- ppLayout =
      --   white
      --     . ( \case
      --           "Spacing Tabbed Tall" -> "<icon=tiled.xpm/>"
      --           "Mirror Spacing Tabbed Tall" -> "<icon=mirrortiled.xpm/>"
      --           "Full" -> "<icon=full.xpm/>"
      --           "monocle" -> "<icon=monocle.xpm/>"
      --           "Spacing ThreeCol" -> "<icon=threecol.xpm/>"
      --           "Tabbed Simplest" -> "<icon=tabbed.xpm/>"
      --           "BSP" -> "<icon=bsp.xpm/>"
      --       )
    }
  where
    grey = xmobarColor "#6c7086" ""
    white = xmobarColor "#ffffff" ""
    blue = xmobarColor "#89b4fa" ""
