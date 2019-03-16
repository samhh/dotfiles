#!/bin/bash

amixer sget Master | grep off > /dev/null
is_muted=$?

if (( is_muted == 0 )); then
  volume=0%
else
  volume=$(amixer sget Master | grep % | awk -F '[][]' '{ print $2 }')
fi

# treat both real mute and 0% sound as being muted
if [[ $volume == "0%" ]]; then
  class=muted
fi

echo -e "$volume\n\n$class"

