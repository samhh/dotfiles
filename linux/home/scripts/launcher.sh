#!/usr/bin/env bash

shopt -s nullglob globstar

script=${1}
window_title=${2}

$(alacritty --dimensions 64 16 --title $window_title -e sh -c "$script")

