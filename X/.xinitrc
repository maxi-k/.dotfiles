#!/bin/sh

[ -f /etc/xprofile ] && [ -z $DOTFILES_XPROFILE_LOADED ] && source /etc/xprofile
[ -f "$HOME/.xprofile" ] && [ -z $DOTFILES_XPROFILE_LOADED ] && source ${HOME}.xprofile

echo "xinitrc loaded" >> ~/log
