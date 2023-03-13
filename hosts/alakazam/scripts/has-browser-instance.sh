#!/bin/sh

output="$1"
profile="$2"

id=$(swaymsg -t get_workspaces | jq ".[] | select(.focused) | .id")

is_active_ws=".type==\"workspace\" and .id==$id"
ws=$(
  swaymsg -t get_tree |
  jq ".nodes[] | select(.name==\"$output\") | .nodes[] | select($is_active_ws)"
)

# Requires the following addon to be configured accordingly:
#   https://addons.mozilla.org/en-GB/firefox/addon/window-titler/
if [ -z "$profile" ]; then
  prefix="\[P: default] "
else
  prefix="\[P: $profile] "
fi

echo "$ws" | gron | grep -iE "json\.(nodes\[[0-9]+]\.)+name = \"$prefix.+\""
