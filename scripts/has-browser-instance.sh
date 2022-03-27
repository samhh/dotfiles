#!/bin/sh

output="$1"
profile="$2"

id=$(swaymsg -t get_workspaces | jq ".[] | select(.focused) | .id")

is_active_ws=".type==\"workspace\" and .id==$id"
ws=$(
  swaymsg -t get_tree |
  jq ".nodes[] | select(.name==\"$output\") | .nodes[] | select($is_active_ws)"
)

is_qute=".app_id==\"org.qutebrowser.qutebrowser\""
# All browser instance window titles have the form "<page title> - qutebrowser",
# however only those launched via qbpm have an additional "(<profile name>)"
# suffix. We'll assume that an empty `$profile` denotes the default profile and
# adjust the check accordingly.
if [ -z "$profile" ]; then
  # e.g. "page - qutebrowser"
  is_profile="(.name | endswith(\"qutebrowser\"))"
else
  # e.g. "page - qutebrowser (profile)"
  is_profile="(.name | endswith(\"($profile)\"))"
fi
echo "$ws" | jq ".nodes | any($is_qute and $is_profile)"
