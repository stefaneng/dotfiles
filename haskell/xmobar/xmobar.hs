-- Author:  Stefan Eng
-- License: GPL v3
-- File:    xmobar.hs
-- Description:
--     xmobar configuration file

Config { font = "xft:DejaVu-10"
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
       , commands = [ Run Date "%a %b %_d %Y - %H:%M:%S" "theDate" 10
                    , Run BatteryP ["BAT1"]
                              ["-t", "<acstatus> <timeleft> (<left>%)",
                               "-L", "10", "-H", "80", "-p", "3",
                               "--", "-O", "<fc=green>On</fc> - ", "-i", "",
                               "-o", "<fc=red>Off</fc> - ", "-i", "",
                               "-L", "-15", "-H", "-5",
                               "-l", "red", "-m", "blue", "-h", "green"]
                              30
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ [%battery%] - <fc=#EE0000>%theDate%</fc>"
       }
