#!/usr/bin/env bash

shopt -s nullglob globstar

# accept optional argument or use this default
friendly_name=${1:-'password'}

# this is largely derived from the passmenu script distributed with pass
base_path=${PASSWORD_STORE_DIR-~/.password-store}
all_paths=( "$base_path"/**/*.gpg )
all_paths=( "${all_paths[@]#"$base_path"/}" )
all_paths=( "${all_paths[@]%.gpg}" )

selected_path=$(printf '%s\n' "${all_paths[@]}" | rofi -dmenu "$@" -p "$friendly_name")

[[ -n $selected_path ]] || exit

pass show -c "$selected_path"

