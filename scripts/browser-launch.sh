#!/bin/sh

# This could/should come from Sway, but that's a lot of argument threading
# through shell scripts of all things.
output="DP-3"
query="$1"
profile="$2"

if [ -z "$output" ]; then
  app="qbpm launch $profile"
else
  app="qutebrowser"
fi

if [ $("$(dirname "$0")/has-browser-instance.sh" "$output" "$profile") = "true" ]; then
  target="tab"
else
  target="window"
fi

"$app" --target "$target" --untrusted-args "$query"
