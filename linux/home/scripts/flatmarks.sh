#!/usr/bin/env bash

shopt -s nullglob globstar

base=~/.bookmarks/

# Find everything in bookmarks dir not a directory and not matching git pattern
readarray -d '' paths < <(find $base -type f -not -path '*/.git*' -print0)

# Remove prefix from strings
paths=( "${paths[@]#"$base"}" )

selected=$(printf '%s\n' "${paths[@]}" | rofi -window-title bookmark -dmenu)

[[ -n $selected ]] || exit

file="$base$selected"

url=$(head -1 $file)

xdg-open $url

