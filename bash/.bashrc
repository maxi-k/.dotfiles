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
test -s "$HOME/.kiex/scripts/kiex" && source "$HOME/.kiex/scripts/kiex" # Load the kiex elixir version manager

# Load z script
. ~/.dotfiles/bin/z/z.sh


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
