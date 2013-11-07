-- Author:  Stefan Eng
-- License: GPL v3
-- File:    xmonad.hs
-- Description:
--     Xmonad window manager configuration file.

import XMonad

main = do
    xmonad $ defaultConfig
       { modMask = mod4Mask
       , terminal = "urxvt"
       }
