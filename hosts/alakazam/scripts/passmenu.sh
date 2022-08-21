#!/usr/bin/env bash

shopt -s nullglob globstar

# this is largely derived from the passmenu script distributed with pass
base_path=${PASSWORD_STORE_DIR-~/.password-store}
all_paths=( "$base_path"/**/*.gpg )
all_paths=( "${all_paths[@]#"$base_path"/}" )
all_paths=( "${all_paths[@]%.gpg}" )

selected_path=$(printf '%s\n' "${all_paths[@]}" | tofi --prompt 'password ')

[[ -n "$selected_path" ]] || exit

pass show "$selected_path" -c
