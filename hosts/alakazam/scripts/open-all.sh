#!/bin/sh

dir="$1"

bms=$(find ~/bookmarks/"$dir" -type f)

for bm in $bms; do
    head -1 "$bm" | xargs librewolf --new-tab &
done
