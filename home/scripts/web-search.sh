#!/usr/bin/env bash

query=$(rofi -dmenu -l 0 -p web)
[[ -n $query ]] || exit

qutebrowser "$query"

