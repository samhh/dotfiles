#!/usr/bin/env bash

query=$(echo | rofi -dmenu -l 0 -p web)

[[ -n $query ]] || exit

vimb "$query"

