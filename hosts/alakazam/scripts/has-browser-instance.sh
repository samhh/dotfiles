#!/bin/sh

output="$1"
profile="$2"

id=$(swaymsg -t get_workspaces | jq ".[] | select(.focused) | .id")

is_active_ws=".type==\"workspace\" and .id==$id"
ws=$(
  swaymsg -t get_tree |
  jq ".nodes[] | select(.name==\"$output\") | .nodes[] | select($is_active_ws)"
)

# All browser instance window titles have the form "<page title> - qutebrowser",
# however only those launched via qbpm have an additional "(<profile name>)"
# suffix. We'll assume that an empty `$profile` denotes the default profile and
# adjust the check accordingly.
if [ -z "$profile" ]; then
  suffix=" - +qutebrowser"
else
  suffix=" - +qutebrowser \($profile\)"
fi

echo "$ws" | gron | grep -E "json\.(nodes\[[0-9]+]\.)+name = \".+$suffix\""
