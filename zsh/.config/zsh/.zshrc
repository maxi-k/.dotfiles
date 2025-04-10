#!/bin/zsh

test "$TERM" = "dumb" && unsetopt zle && export PS1="$ " && return

export GPG_TTY=$(tty)

# Load in look config, aliases and local config
test -f ~/.config/zsh/lookrc &&  source ~/.config/zsh/lookrc
test -f ~/.config/zsh/keyrc && source ~/.config/zsh/keyrc
test -f ~/.config/zsh/toolrc && source ~/.config/zsh/toolrc
test -f ~/.config/shell/profile && source ~/.config/shell/profile
test -f ~/.config/shell/aliasrc && source ~/.config/shell/aliasrc
test -f ~/.config/shell/localrc && source ~/.config/shell/localrc

# Load z script for entering recent folders
test -f ~/.dotfiles/bin/z/z.sh && . ~/.dotfiles/bin/z/z.sh

setopt sharehistory
HISTSIZE=5000
HISTFILE=~/.cache/.zsh_history     
SAVEHIST=100000               
HISTDUP=erase              
setopt appendhistory     # Append history to the history file (no overwriting)
setopt sharehistory      # Share history across terminals
setopt incappendhistory  # Immediately append to the history file, not just when a term is killed

[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '

# Simple calculator
calc() {
        local result=""
        result="$(printf "scale=10;$*\n" | bc --mathlib | tr -d '\\\n')"
        #                       └─ default (when `--mathlib` is used) is 20
        #
        if [[ "$result" == *.* ]]; then
                # improve the output for decimal numbers
                printf "$result" |
                sed -e 's/^\./0./'        `# add "0" for cases like ".5"` \
                    -e 's/^-\./-0./'      `# add "0" for cases like "-.5"`\
                    -e 's/0*$//;s/\.$//'   # remove trailing zeros
        else
                printf "$result"
        fi
        printf "\n"
}

# enable autocompletion
autoload -U compinit; compinit

zstyle ':completion:*' completer _extensions _complete _approximate
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/.zcompcache"
zstyle ':completion:*' menu select
zstyle ':completion:*' group-name ''
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# load plugin config if it exists
#test -f ~/.config/zsh/pluginrc && source ~/.config/zsh/pluginrc
