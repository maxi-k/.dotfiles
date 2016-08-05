export LC_ALL=en_US.utf-8
export LANG="$LC_ALL"

test -f ~/.dotfiles/.bash_look && source ~/.dotfiles/.bash_look

test -f ~/.search/search_wrapper.sh && source ~/.search/search_wrapper.sh

# Load in aliases
test -f ~/.dotfiles/.bash_aliases && source ~/.dotfiles/.bash_aliases

# Load .bash_local if it exists
test -f ~/.dotfiles/.bash_local && source ~/.dotfiles/.bash_local
