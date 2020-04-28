#!/bin/sh

[ -d ${HOME}/.dotfiles/bin ] && export PATH="${HOME}/.dotfiles/bin:$PATH"
[ -e ${HOME}/.config/X/default-config ] && xrdb ${HOME}/.config/X/default-config

pgrep dunst || dunst &  # Dunst the messaging service
mktheme & # Create a theme for this session
[ -e ${HOME}/.config/polybar/start.sh ] && ${HOME}/.config/polybar/start.sh

export _JAVA_AWT_WM_NONREPARENTING=1 
