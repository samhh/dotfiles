#!/usr/bin/env bash

base=~/bookmarks-work/

# Find everything in bookmarks dir not a directory and not matching git pattern
readarray -d '' paths < <(find $base -type f -not -path '*/.git*' -print0)

# Remove prefix from strings
paths=( "${paths[@]#"$base"}" )

selected=$(printf '%s\n' "${paths[@]}" | dmenu -p bookmark-work)

[[ -n $selected ]] || exit

file="$base$selected"

url=$(head -1 "$file")

"$(dirname "$0")/browser-launch.sh" "$url" unsplash
