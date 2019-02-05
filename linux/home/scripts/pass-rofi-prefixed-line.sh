#!/usr/bin/env bash

shopt -s nullglob globstar

# accept optional arguments or use these defaults
prefix=${1:-'login: '}
friendly_name=${2:-'login'}

# this is largely derived from the passmenu script distributed with pass
base_path=${PASSWORD_STORE_DIR-~/.password-store}
all_paths=( "$base_path"/**/*.gpg )
all_paths=( "${all_paths[@]#"$base_path"/}" )
all_paths=( "${all_paths[@]%.gpg}" )

selected_path=$(printf '%s\n' "${all_paths[@]}" | rofi -dmenu "$@" -p "$friendly_name")

[[ -n $selected_path ]] || exit

regex="^$prefix"

full_file=$(pass show $selected_path)
mapfile -t lines <<< "$full_file"

for line in "${lines[@]}"; do
  # returns the username if successful or an empty string if not
  result=$(echo $line | sed -ne "s/$regex//p")

  if [[ ! -z $result ]]; then
    echo $result | xclip -selection clipboard
    echo "Copied $friendly_name from $selected_path to clipboard."

    break
  fi
done

