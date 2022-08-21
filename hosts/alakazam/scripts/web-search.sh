#!/bin/sh

query=$(echo "" | tofi --prompt web)

if [ -z "$query" ]; then exit 1; fi

"$(dirname "$0")/browser-launch.sh" "$query"
