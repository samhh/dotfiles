#!/bin/bash

MESSAGE="Low battery."
PERC_LIMIT=10

BATTERY="BAT0"
PATH="/sys/class/power_supply/$BATTERY"

FULL=$(/usr/bin/cat $PATH/charge_full)
REM=$(/usr/bin/cat $PATH/charge_now)
STAT=$(/usr/bin/cat $PATH/status)

PERC=$((($REM * 100) / $FULL))

# If not charging and battery is below percentage limit defined above, then send notification
if [ $STAT != "Charging" ] && [ $PERC -le $PERC_LIMIT ]; then
  /usr/bin/notify-send -u critical -a "system" "Low battery!" "Plug in a charger ASAP!"
fi
