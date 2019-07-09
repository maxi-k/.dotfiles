export LC_ALL=en_US.utf-8
export LANG="$LC_ALL"
export VISUAL="vim"
export EDITOR="emacsclient -nw"

test -f ~/.bash_look && source ~/.bash_look

test -f ~/.search/search_wrapper.sh && source ~/.search/search_wrapper.sh

# Load in aliases
test -f ~/.bash_aliases && source ~/.bash_aliases

# Load .bash_local if it exists
test -f ~/.bash_local && source ~/.bash_local
# test -s "$HOME/.kiex/scripts/kiex" && source "$HOME/.kiex/scripts/kiex" # Load the kiex elixir version manager

# Use the extensible version manager asdf
if [ -x "$(command -v brew)" ]; then
  # Too slow, calling brew
  ### ASDF_DATA_DIR=`brew --prefix asdf`
  # instead, setup manually
  export ASDF_DATA_DIR="/usr/local/opt/asdf"
elif [ -d "/opt/asdf-vm" ]; then
  export ASDF_DATA_DIR="/opt/asdf-vm"
else
  export ASDF_DATA_DIR="$HOME/.asdf"
fi
if [ -f "$ASDF_DATA_DIR/asdf.sh" ]; then
  source "$ASDF_DATA_DIR/asdf.sh"
  test -f "$ASDF_DATA_DIR/completions/asdf.bash" && source "$ASDF_DATA_DIR/completions/asdf.bash"
else
  echo "asdf not found"
fi

# Load z script
. ~/.dotfiles/bin/z/z.sh

export PATH=$HOME/.npm/bin:$PATH
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# This loads nvm with bash completion
# export NVM_DIR="$HOME/.nvm"
# [ -s "$(brew --prefix nvm)/nvm.sh" ] && \. "$(brew --prefix nvm)/nvm.sh"
# [ -s "$(brew --prefix nvm)/bash_completion" ] && \. "$(brew --prefix nvm)/bash_completion"
