#!/bin/bash

alias la='ls -a'
alias lla='ls -la'

alias ec='emacsclient -nw'

alias cdc='pwd | pbcopy'
alias cdp='cd $(pbpaste)'

alias tml="tmux list-sessions"
alias tma="tmux -2 attach -t $1"
alias tmk="tmux kill-session -t $1"

alias stack-watch="stack build --file-watch --copy-bins"
