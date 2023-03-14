#!/bin/sh

# This could/should come from Sway, but that's a lot of argument threading
# through shell scripts of all things.
output="DP-3"
query="$1"
profile="$2"

isUnsplashAddr=$(case "$query" in
  *figma.com*|*linear.app*|*trello.com*|*gettyimages*|*sentry.io*|*datadoghq.com*) echo "yep" ;;
esac)

if [ "$isUnsplashAddr" ]; then
  app="librewolf -P unsplash"
elif [ -n "$profile" ]; then
  app="librewolf -P $profile"
else
  app="librewolf"
fi

if [ "$("$(dirname "$0")/has-browser-instance.sh" "$output" "$profile")" ]; then
  target="tab"
else
  target="window"
fi

# Unquoted `$app` is intentional.
$app --new-"$target" "$query"
