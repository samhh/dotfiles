#!/usr/bin/env bash

cmd="$1"
[[ -n $cmd ]] || exit

time="$2"
[[ -n $time ]] || exit

while true; do "$cmd"; sleep "$time"; done

