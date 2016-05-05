source ~/.search/search_wrapper.sh
alias la='ls -a'
alias lla='ls -la'

alias ec='emacsclient -nw'

alias cdc='pwd | pbcopy'
alias cdp='cd $(pbpaste)'

alias tml="tmux list-sessions"
alias tma="tmux -2 attach -t $1"
alias tmk="tmux kill-session -t $1"

export LC_ALL=en_US.utf-8
export LANG="$LC_ALL"
