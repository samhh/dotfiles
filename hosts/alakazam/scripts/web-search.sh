#!/bin/sh

query=$(echo "" | tofi --prompt 'web ' --require-match false)

if [ -z "$query" ]; then exit 1; fi

"$(dirname "$0")/browser-launch.sh" "$query"
