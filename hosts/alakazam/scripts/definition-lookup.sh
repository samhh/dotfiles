#!/bin/sh

"$(dirname "$0")/browser-launch.sh" "http://localhost:1234/?q=$(wl-paste -p)!d"
