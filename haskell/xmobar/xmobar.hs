-- Author:  Stefan Eng
-- License: GPL v3
-- File:    xmobar.hs
-- Description:
--     xmobar configuration file

Config { font = "xft:DejaVu-10"
       , bgColor = "#000000"
       , fgColor = "#BFBFBF"
       , position = Top
       , hideOnStart = False
       , lowerOnStart = True
       , persistent = False
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run Date "%a %b %_d %Y - %H:%M:%S" "theDate" 10
                    , Run BatteryP ["BAT1"]                          
                              ["-t", "<fc=white>Battery:</fc> <acstatus> <left>% <timeleft>",
                               "-L", "10", "-H", "80", "-p", "3",
                               "--", "-O", "<fc=green>Charging</fc> - ", "-i", "",
                               "-o", "<fc=yellow>Discharging</fc> -", "-i", "",
                               "-L", "-15", "-H", "-5",
                               "-l", "red", "-m", "blue", "-h", "green"] 30
                    , Run Wireless "wlp1s0" [ "-t", "<fc=white>Wireless:</fc> <essid> <quality>%"] 
                              10   
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %wlp1s0wi% | %battery% | <fc=#EE0000>%theDate%</fc>"
       }
