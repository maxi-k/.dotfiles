#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

# Start on multiple monitors
# --------------------------
## # Get list of all monitors
## monitors=$(xrandr | grep " connected" | awk '{ print $1 }')
## # Launch polybar on all monitors
## for m in $monitors
## do
##   MONITOR=$m polybar bottom &
## done

MONITOR=$(xrandr | grep "primary" | awk '{ print $1 }') polybar bottom &
