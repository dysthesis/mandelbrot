{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Config.XMobar (withStatusBars) where

import Graphics.X11.Types (Window)
import XMonad (KeyMask, KeySym, Layout, XConfig (XConfig, modMask), xK_b)
import XMonad.Core (LayoutClass, ScreenId (S))
import XMonad.Hooks.ManageDocks (AvoidStruts)
import XMonad.Hooks.StatusBar (StatusBarConfig, dynamicSBs, statusBarProp)
import XMonad.Hooks.StatusBar.PP (PP (ppCurrent, ppHidden, ppLayout, ppSep, ppTitle, ppTitleSanitize, ppVisible, ppWsSep), def, filterOutWsPP, shorten, wrap, xmobarBorder, xmobarColor, xmobarStrip)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)

withStatusBars :: (LayoutClass l Window) => XConfig l -> XConfig l
withStatusBars = dynamicSBs barSpawner

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner = pure . xmobar
  where
    xmobar :: ScreenId -> StatusBarConfig
    xmobar (S screenId) = statusBarProp ("xmobar-configured -x " <> show screenId) $ pure (filterOutWsPP [scratchpadWorkspaceTag] pp)
    pp :: PP
    pp =
        def
            { ppSep = grey "  \xf01d9  "
            , ppCurrent = blue . wrap "" "" . xmobarBorder "Top" "#7788AA" 3
            , -- , ppCurrent = blue
              ppHidden = grey
            , ppVisible = white
            , ppWsSep = "  "
            , ppTitleSanitize = xmobarStrip . shorten 30 -- `shorten` defines the max length
            , ppTitle = wrap "\xf0570 " ""
            , ppLayout =
                white
                    . ( \case
                            "Tiled" -> "<icon=tiled.xpm/>"
                            "Mirror Tiled" -> "<icon=mirrortiled.xpm/>"
                            "Full" -> "<icon=full.xpm/>"
                            "monocle" -> "<icon=monocle.xpm/>"
                            "Spacing ThreeCol" -> "<icon=threecol.xpm/>"
                            "Tabbed Simplest" -> "<icon=tabbed.xpm/>"
                            "BSP" -> "<icon=bsp.xpm/>"
                      )
            }
      where
        grey = xmobarColor "#2A2A2A" ""
        white = xmobarColor "#ffffff" ""
        blue = xmobarColor "#7788AA" ""
