import Config.Autostart (myStartupHook)
import Config.Keybinds (DefaultPrograms (..), myKeys)
import Config.Layout (myLayout)
import Config.ManageHook (myManageHook)
import Config.XMobar (xmobarProp)
import Utils.Taskwarrior (taskwarriorKeybinds)
import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (docks)
import XMonad.Hooks.WindowSwallowing (swallowEventHook)
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Hacks (windowedFullscreenFixEventHook)

{-- VARIABLES:
 - Define some basic settings for XMonad here. This includes the modifier keys, default terminal emulator, window borders, etc. --}
myModMask :: KeyMask
myModMask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 1

myNormColor :: String
myNormColor = "#0f0f0f"

myFocusColor :: String
myFocusColor = "#FFFFFF"

defaultPrograms :: DefaultPrograms
defaultPrograms =
  DefaultPrograms
    { term = "ghostty",
      launcher = "dmenu_run",
      editor = "nvim",
      screenshot = "flameshot gui"
    }

myHandleEventHook = windowedFullscreenFixEventHook <> swallowEventHook (className =? "st-256color" <||> className =? "Alacritty") (return True)

myConfig =
  def
    { modMask = myModMask,
      terminal = term defaultPrograms,
      borderWidth = myBorderWidth,
      normalBorderColor = myNormColor,
      focusedBorderColor = myFocusColor,
      layoutHook = myLayout,
      -- , workspaces = myWorkspaces
      startupHook = myStartupHook,
      manageHook = myManageHook,
      handleEventHook = myHandleEventHook
    }
    `additionalKeysP` myKeys defaultPrograms

main :: IO ()
main =
  do
    xmonad
    . docks
    . ewmhFullscreen
    . ewmh
    . fullscreenSupport
    $ xmobarProp myConfig
