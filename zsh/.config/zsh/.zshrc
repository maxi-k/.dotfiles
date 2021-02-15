#!/bin/zsh

export GPG_TTY=$(tty)

# Load in look config, aliases and local config
test -f ~/.config/zsh/lookrc &&  source ~/.config/zsh/lookrc
test -f ~/.config/zsh/keyrc && source ~/.config/zsh/keyrc
test -f ~/.config/shell/aliasrc && source ~/.config/shell/aliasrc
test -f ~/.config/shell/localrc && source ~/.config/shell/localrc

# Load z script for entering recent folders
test -f ~/.dotfiles/bin/z/z.sh && . ~/.dotfiles/bin/z/z.sh

setopt sharehistory
HISTSIZE=5000               
HISTFILE=~/.cache/.zsh_history     
SAVEHIST=5000               
HISTDUP=erase              
setopt appendhistory     # Append history to the history file (no overwriting)
setopt sharehistory      # Share history across terminals
setopt incappendhistory  # Immediately append to the history file, not just when a term is killed


test -e $HOME/.nix-profile/etc/profile.d/nix.sh && source $HOME/.nix-profile/etc/profile.d/nix.sh 
