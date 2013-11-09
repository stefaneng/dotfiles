-- Author:  Stefan Eng
-- License: GPL v3
-- File:    xmonad.hs
-- Description:
--     Xmonad window manager configuration file.

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.Cursor (setDefaultCursor, xC_left_ptr)

main = do
  handle <- spawnPipe "xmobar"
  xmonad $ defaultConfig
             { modMask     = myModMask
             , terminal    = myTerminal
             , manageHook  = myManageHook
             , layoutHook  = myLayoutHook
             , startupHook = myStartupHook
             , logHook     = dynamicLogWithPP xmobarPP 
                             { ppOutput = hPutStrLn handle }
             }

-- the mod modifier
-- set to windows key
myModMask :: KeyMask
myModMask = mod4Mask

-- use rxvt-unicode as terminal
-- config file in dotfiles/X/Xresources
myTerminal :: String
myTerminal = "urxvt"

-- not quite sure exactly what the options are for manageHook
myManageHook :: ManageHook
myManageHook = manageDocks <+> manageHook defaultConfig

-- same goes for the layoutHook
myLayoutHook = avoidStruts $ layoutHook defaultConfig

-- this should probably go into an X config file and not here
setMyCursor :: X ()
setMyCursor = setDefaultCursor xC_left_ptr

-- startup hook runs when xmonad starts
myStartupHook :: X ()
myStartupHook = setMyCursor

-- dynamic logging for xmobar
-- pass a handle to it
--myLogHook :: X ()
--myLogHook handle = dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn handle }
