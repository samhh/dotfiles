#!/usr/bin/env bash

target=${1:-'tab'}

base=~/bookmarks/

# Find everything in bookmarks dir not a directory and not matching git pattern
readarray -d '' paths < <(find $base -type f -not -path '*/.git*' -print0)

# Remove prefix from strings
paths=( "${paths[@]#"$base"}" )

selected=$(printf '%s\n' "${paths[@]}" | bemenu -p bookmark)

[[ -n $selected ]] || exit

file="$base$selected"

url=$(head -1 $file)

qutebrowser --target "$target" --untrusted-args "$url"
