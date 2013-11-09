-- Author:  Stefan Eng
-- License: GPL v3
-- File:    xmobar.hs
-- Description:
--     xmobar configuration file

Config { font = "xft:Inconsolata-10"
       , bgColor = "#000000"
       , fgColor = "#BFBFBF"
       , position = Top
       , border = NoBorder
       , borderColor = "#BFBFBF"
       , hideOnStart = False
       , lowerOnStart = True
       , persistent = False
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run Date "%a %b %_d %Y * %H:%M:%S" "theDate" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ <fc=#00FF00>%uname%</fc> * <fc=#FF0000>%theDate%</fc>"
       }
