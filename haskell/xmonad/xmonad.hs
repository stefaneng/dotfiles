-- Author:  Stefan Eng
-- License: GPL v3
-- File:    xmonad.hs
-- Description:
--     Xmonad window manager configuration file.

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Cursor

main = xmonad =<< xmobar myConfig

myConfig = defaultConfig
           { modMask    = mod4Mask -- Use windows key for mod
           , terminal   = "urxvt"
           , manageHook = manageDocks <+> manageHook defaultConfig
           , layoutHook = avoidStruts  $ layoutHook defaultConfig           
           , startupHook = setDefaultCursor xC_left_ptr
           }

