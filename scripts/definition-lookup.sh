#!/bin/sh

"$(dirname "$0")/browser-launch.sh" "$(wl-paste -p | sed 's/$/!d/')"
