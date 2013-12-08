# Author:      Stefan Eng
# License:     GPL v3
# File:        bashrc
# Description:
#     This file is sourced by all *interactive* bash shells on startup,
#     including some apparently interactive shells such as scp and rcp
#     that can't tolerate any output.  So make sure this doesn't display
#     anything or bad things will happen !


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
[[ $- != *i* ]] && return

# Access colors with normal names
[[ -f ~/.bash_colors ]] && . ~/.bash_colors

# Source functions
[[ -f ~/.bash_functions ]] && . ~/.bash_functions

# Source the aliases
[[ -f ~/.bashrc ]] && . ~/.bash_aliases

# Source bash completion, not sure why it is not working
[[ -f /etc/profile.d/bash-completion.sh ]] && . /etc/profile.d/bash-completion.sh

# Remove duplicates from history
export HISTCONTROL="$HISTCONTROL erasedups:ignoreboth"

# Makes redirect not overwrite file
set -o noclobber

# Prompt, use double quotes to expand colors and \[ \] escapes them
# See colors in bash/colors
PS1="\[$BGreen\]\u@\h\[$BBlue\] \[$BBlue\]\w \[$BRed\]\$(git_branch) \[$Green\]\$ \[$Color_Off\]"

# Add things to the path here
##
# Add cabal to the path
export PATH=~/.cabal/bin:$PATH

# Add Ruby Gems to path
export PATH=~/.gem/ruby/2.0.0/bin:$PATH

# Add ~/bin to path
export PATH=~/bin:$PATH
