#!/usr/bin/env bash

target=${1:-'tab'}

query=$(echo '' | bemenu 0 -p web)
[[ -n $query ]] || exit

qutebrowser --target "$target" --untrusted-args "$query"
