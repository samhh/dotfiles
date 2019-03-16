#!/bin/bash

player_status=$(playerctl status 2> /dev/null)
if [[ $? -eq 0 ]]; then
  metadata="$(playerctl metadata artist) - $(playerctl metadata title)"
fi

if [[ $player_status = "Playing" || $player_status = "Paused" ]]; then
  echo $metadata
else
  exit 1
fi

