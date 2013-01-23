#!/bin/sh
# Turn off ATI card TimeLineX 3820TG

# Check if root
if [ ! $UID -eq 0 ]; then
  echo "must be root"
  exit 1
fi

# Check if debugfs mounted
if [ ! -d "/sys/kernel/debug" ]; then
  mount -t debugfs debugfs /sys/kernel/debug
fi

# make sure radeon driver is loaded
modprobe radeon

# turn off ATI card
echo OFF > /sys/kernel/debug/vgaswitcheroo/switch


case "$1" in
  "") ;;
  "-v") cat /sys/kernel/debug/vgaswitcheroo/switch ;;
  *) echo "Only argument -v available" ;; 
esac

exit
