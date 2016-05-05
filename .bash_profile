
# Setting PATH for Python 2.7
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
export PATH

# Setting the path for the go programming language
GOPATH="${HOME}/Applications/bin/go"
export GOPATH

# Setting GROOVY_HOME
GROOVY_HOME=/usr/local/opt/groovy/libexec

# Set architecture flags
export ARCHFLAGS="-arch x86_64"
# Ensure user-installed binaries take precedence
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin

# Set up mysql into the path
MYSQL=/usr/local/mysql/bin
export PATH=$PATH:$MYSQL
export DYLD_LIBRARY_PATH=/usr/local/mysql/lib:$DYLD_LIBRARY_PATH

# Add the 'z' script (find files quickly) to bash
. `brew --prefix`/etc/profile.d/z.sh

# Load .bashrc if it exists
test -f ~/.bashrc && source ~/.bashrc

# git branch in prompt

parse_git_branch() {

    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'

}

# Make the prompt look nice
# (User Directory git-branch $) with colors (grey yellow yellow grey)
c_grey=$(tput setaf 8)
c_yellow=$(tput setaf 220)
c_reset=$(tput sgr0)
export PS1="\[$c_grey\]\u\[$c_reset\] \[$c_yellow\]\W\$(parse_git_branch)\[$c_reset\] \[$c_grey\]$\[$c_reset\] "

# Add my scripts to the path
if [ -d "${HOME}/Applications/scripts" ]; then
    export PATH="${HOME}/Applications/scripts:${PATH}"
fi

# Add user local bin files to the path
if [ -d "${HOME}/Applications/bin" ]; then
    export PATH="${HOME}/Applications/bin:${PATH}"
fi


# Add the gemdirs to the path
export PATH="/usr/local/lib/ruby/gems/2.2.0:/usr/local/Cellar/ruby/2.2.3/lib/ruby/gems/2.2.0:/Users/Maxi/.gem/ruby/2.2.0:${PATH}"

# Add rbenv to the path
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"


# Add Haskell stuff
export PATH="/Users/Maxi/Library/Haskell/ghc-7.10.3/lib/elm-init-1.0.1.1/bin:${PATH}"


# Set the default editor to emacs-client
export EDITOR="emacsclient -nw"

# Only load Liquid Prompt in interactive shells, not from a script or from scp
# [[ $- = *i* ]] && source ~/.liquidprompt/liquidprompt
