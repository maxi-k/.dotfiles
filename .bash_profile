# Set architecture flags
export ARCHFLAGS="-arch x86_64"

# Ensure user-installed binaries take precedence
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH

# Set up mysql into the path
MYSQL=/usr/local/mysql/bin
export PATH=$PATH:$MYSQL
export DYLD_LIBRARY_PATH=/usr/local/mysql/lib:$DYLD_LIBRARY_PATH

# Add my scripts to the path
test -d ~/Applications/scripts && export PATH=${HOME}/Applications/scripts:${PATH}

# Add user local bin files to the path
test -d ~/Applications/bin && export PATH=~/Applications/bin:${PATH}

# Add the local bin folder to the PATH
test -d ~/.local/bin && export PATH=~/.local/bin/:${PATH}

# Set the default editor to emacs-client
export EDITOR="emacsclient -nw"

# Load .bashrc if it exists
test -e ~/.bashrc && source ~/.bashrc

# Load .bash_local if it exists
test -f ~/.dotfiles/.bash_local && source ~/.dotfiles/.bash_local