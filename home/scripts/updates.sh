#!/bin/sh

updates_arch=$(checkupdates 2> /dev/null | wc -l)
updates_aur=$(yay -Qum 2> /dev/null | wc -l)
updates_total=$(( "$updates_arch" + "$updates_aur" ))

if (( updates_total == 0 )); then exit 1; fi

echo $updates_total


