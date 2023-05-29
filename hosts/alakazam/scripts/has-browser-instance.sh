#!/bin/sh

output="$1"

id=$(swaymsg -t get_workspaces | jq ".[] | select(.focused) | .id")

is_active_ws=".type==\"workspace\" and .id==$id"
ws=$(
  swaymsg -t get_tree |
  jq ".nodes[] | select(.name==\"$output\") | .nodes[] | select($is_active_ws)"
)

echo "$ws" | gron | grep -iE "json\.(nodes\[[0-9]+]\.)+name = \".*(LibreWolf|Mozilla Firefox)\""
