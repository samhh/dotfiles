#!/bin/sh

cfg="${XDG_CONFIG_HOME:-$HOME/.config}"/bangin/bangin.lists
output_dir="${XDG_DATA_HOME:-$HOME/.local/share}"/bangin/lists/

rm -rf "$output_dir"
mkdir -p "$output_dir"

lists=$(cat "$cfg" 2> /dev/null)

for list in $lists; do
  curl -s "$list" > "$output_dir/$(basename "$list")"
done
