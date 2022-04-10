#!/bin/sh

dir="$1"

kill $(pgrep swaybg) 2> /dev/null

find "$dir" -type f | shuf -n1 | xargs swaybg -m fill -i
