#!/bin/sh

# This could/should come from Sway, but that's a lot of argument threading
# through shell scripts of all things.
output="DP-3"
query="$1"

if [ "$("$(dirname "$0")/has-browser-instance.sh" "$output")" ]; then
  target="tab"
else
  target="window"
fi

librewolf --new-"$target" "$query"
