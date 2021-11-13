#!/bin/bash

target=${1:-'tab'}

query=$(rofi -dmenu -l 0 -p web)
[[ -n $query ]] || exit

qutebrowser --target "$target" --untrusted-args "$query"

