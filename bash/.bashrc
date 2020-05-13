export LC_ALL=en_US.UTF-8
export LANG="$LC_ALL"
export VISUAL="vim"
export EDITOR="emacsclient -nw"
export GPG_TTY=$(tty)

# Load in look config, aliases and local config
test -f ~/.config/bash/lookrc &&  source ~/.config/bash/lookrc
test -f ~/.config/shell/aliasrc && source ~/.config/shell/aliasrc 
test -f ~/.config/shell/localrc && source ~/.config/shell/localrc 

test -f ~/.search/search_wrapper.sh && source ~/.search/search_wrapper.sh

# Use the extensible version manager asdf
if [ -x "$(command -v brew)" ]; then
  export ASDF_DATA_DIR="/usr/local/opt/asdf"
elif [ -d "/opt/asdf-vm" ]; then
  export ASDF_DATA_DIR="/opt/asdf-vm"
else
  export ASDF_DATA_DIR="$HOME/.asdf"
fi

[ -f "$ASDF_DATA_DIR/asdf.sh" ] && source "$ASDF_DATA_DIR/asdf.sh" || echo "asdf not found"
# [ -f "$ASDF_DATA_DIR/completions/asdf.bash" ] && source "$ASDF_DATA_DIR/completions/asdf.bash" || echo "asdf completions not found"

# Load z script
. ~/.dotfiles/bin/z/z.sh
