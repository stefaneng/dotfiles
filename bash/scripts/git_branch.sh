#!/bin/bash

# Author:      Stefan Eng
# License:     GPL v3
# File:        git_branch
# Description:
#     Gets the git branch for current directory
#     Prints it with the format: (git:branch)

function git_branch() {
    git branch --no-color 2>/dev/null | awk '/\*/ {print "(git:"$2")"}'
}
