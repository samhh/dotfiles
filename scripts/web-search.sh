#!/bin/sh

query=$(echo "" | dmenu -p web)

if [ -z $query ]; then exit 1; fi

"$(dirname "$0")/browser-launch.sh" "$query"
