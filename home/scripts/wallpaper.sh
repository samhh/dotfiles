#!/bin/sh

path=$(~/scripts/private/unsplash.sh)
h=$(date '+%H')

if [[ "$h" -gt 04 && "$h" -lt 17 ]]; then
  wal -l -i "$path"
else
  wal -i "$path"
fi
