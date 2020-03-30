#!/bin/bash

## General Shortcuts
# ls
alias la='ls -a'
alias lla='ls -la'
# copy current directory to clipboard
alias cdc='pwd | pbcopy'
# switch to directory from clipboard
alias cdp='cd $(pbpaste)'

## Emacs shortcuts
alias ec='emacsclient -nw'

## Tmux shortcuts
alias tml="tmux list-sessions" # list sessions
alias tma="tmux -2 attach -t $1" # attach session
alias tmk="tmux kill-session -t $1" # kill session

## Haskell
alias stack-watch="stack build --file-watch --copy-bins"

## Java
# Version management using jabba
alias jvm='jabba'
