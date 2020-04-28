#!/usr/bin/env sh

# Terminate already running bar instances
mkdir -p ~/.cache/polybar
killall -q polybar

# Wait until the processes have been shut down
echo "Stopping old polybar processes."
while pgrep -x polybar >/dev/null; do sleep 1; done

# Start on multiple monitors
# --------------------------
# Get list of all monitors
monitors=$(xrandr | grep " connected" | awk '{ print $1 }')
# # Launch polybar on all monitors
echo "Found $monitors"
for m in $monitors
do
  echo "Activating polybar on monitor $m"
  MONITOR=$m polybar bottom >~/.cache/polybar/log &
done
echo "Activated polybar on all monitors"

# MONITOR=$(xrandr | grep "primary" | awk '{ print $1 }') \
# polybar bottom &
