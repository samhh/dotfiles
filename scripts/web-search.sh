#!/bin/sh

query=$(echo "" | bemenu 0 -p web)

if [ -z $query ]; then exit 1; fi

"$(dirname "$0")/browser-launch.sh" "$query"
