#!/bin/sh

access_key=$(pass misc/_unsplash-access-key)

data_dir="${XDG_DATA_HOME:-$HOME/.local/share}/unsplash"
mkdir -p "$data_dir"

output=$(swaymsg -t get_outputs | jq ".[] | select (.focused) | .current_mode")
w=$(echo "$output" | jq '.width')
h=$(echo "$output" | jq '.height')

res=$(curl -s "https://api.unsplash.com/photos/random?orientation=landscape" -H "Authorization: Client-ID $access_key")
id=$(echo "$res" | jq -r '.id')
url_raw=$(echo "$res" | jq -r '.urls.raw')
url="$url_raw&w=$w&h=$h"

path="$data_dir/$id.jpg"
curl -s "$url" > "$path"

echo "$path"
