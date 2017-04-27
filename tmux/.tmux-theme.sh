tm_icon="☁ "

tm_active_fg=colour110
tm_active_bg=colour237
tm_active_bg_low=colour240
tm_inactive_fg=colour244
tm_inactive_bg=colour235
tm_feature_bg=$tm_inactive_bg
tm_feature_fg=colour180
tm_feature_pop=colour33

tm_tunes="#[fg=$tm_feature_fg]#(osascript ~/.dotfiles/applescript/tunes.scpt)"
tm_battery="#(~/.dotfiles/bin/battery_indicator.sh)"
tm_date="#[fg=$tm_inactive_fg] %R %d %b"
tm_host="#[fg=$tm_feature_fg,bold]#h"
tm_session_name="#[fg=$tm_feature_fg,bold]$tm_icon #S"

set -g status-left ' '$tm_session_name' '
set -g status-right $tm_tunes' '$tm_date' '$tm_host

set -g status-left-length 32
set -g status-right-length 150
set -g status-interval 5

set -g window-status-format " #I:#W "

# default statusbar colors
set-option -g status-bg $tm_inactive_bg #base02
set-option -g status-left-bg $tm_feature_bg #base02
set-option -g status-left-fg $tm_feature_fg

# default window title colors
set-window-option -g window-status-fg $tm_inactive_fg #base0
set-window-option -g window-status-bg $tm_inactive_bg
#set-window-option -g window-status-attr dim

# active window title colors
set-window-option -g window-status-current-format " #[bold]#I:#W "
set-window-option -g window-status-current-fg $tm_active_fg #orange
set-window-option -g window-status-current-bg $tm_active_bg

# pane border
set-option -g pane-border-fg $tm_inactive_bg #base02
set-option -g pane-active-border-fg $tm_active_bg_low

# message text
set-option -g message-bg $tm_inactive_bg #base02
set-option -g message-fg $tm_active_fg #orange

# pane number display
set-option -g display-panes-active-colour $tm_feature_pop
set-option -g display-panes-colour $tm_feature_pop

# clock
set-window-option -g clock-mode-colour $tm_feature_pop
