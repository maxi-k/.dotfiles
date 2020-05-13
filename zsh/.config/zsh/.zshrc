#!/bin/zsh

# General variables 
export LC_ALL=en_US.UTF-8
export LANG="$LC_ALL"
export VISUAL="vim"
export EDITOR="emacsclient -nw"
export GPG_TTY=$(tty)

# Setup PATH 
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:$PATH
export PATH=$PATH:$HOME/.local/bin:$HOME/.dotfiles/bin

# Load in look config, aliases and local config
test -f ~/.config/zsh/lookrc &&  source ~/.config/zsh/lookrc
test -f ~/.config/shell/aliasrc && source ~/.config/shell/aliasrc 
test -f ~/.config/shell/localrc && source ~/.config/shell/localrc 

# Use the extensible version manager asdf
if [ -x "$(command -v brew)" ]; then
  export ASDF_DATA_DIR="/usr/local/opt/asdf"
elif [ -d "/opt/asdf-vm" ]; then
  export ASDF_DATA_DIR="/opt/asdf-vm"
else
  export ASDF_DATA_DIR="$HOME/.asdf"
fi

[ -f "$ASDF_DATA_DIR/asdf.sh" ] && source "$ASDF_DATA_DIR/asdf.sh" || echo "asdf not found"

# Load z script for entering recent folders
test -f ~/.dotfiles/bin/z/z.sh && . ~/.dotfiles/bin/z/z.sh
