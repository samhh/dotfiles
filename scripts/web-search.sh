#!/bin/sh

target=${1:-'tab'}

query=$(echo '' | bemenu 0 -p web)
if [ -z $query ]; then exit 1; fi

qutebrowser --target "$target" --untrusted-args "$query"
