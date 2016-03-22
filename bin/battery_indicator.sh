#!/bin/zsh
# modified from http://ficate.com/blog/2012/10/15/battery-life-in-the-land-of-tmux/

HEART='♥ '

if [[ `uname` == 'Linux' ]]; then
  current_charge=$(upower -i /org/freedesktop/UPower/devices/battery_BAT1 | grep percentage | awk '{print $2}' | sed 's/%//g')
  total_charge=100
else
  battery_info=`ioreg -rc AppleSmartBattery`
  current_charge=$(echo $battery_info | grep -o '"CurrentCapacity" = [0-9]\+' | awk '{print $3}')
  total_charge=$(echo $battery_info | grep -o '"MaxCapacity" = [0-9]\+' | awk '{print $3}')
fi

charged_slots=$(echo "((($current_charge/$total_charge)*10)/3)+1" | bc -l | cut -d '.' -f 1)
#echo $charged_slots

if [[ $charged_slots -gt 10 ]]; then
  charged_slots=10
fi

echo -n '#[fg=red]'
for i in `seq 1 $charged_slots`; do echo -n "$HEART"; done

if [[ $charged_slots -lt 10 ]]; then
  echo -n '#[fg=white]'
  for i in `seq 1 $(echo "10-$charged_slots" | bc)`; do echo -n "$HEART"; done
fi
