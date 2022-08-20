#!/usr/bin/env bash

read -r -d '' apps << fin
chromium
firefox
obsidian
pavucontrol
piper
polymc
qutebrowser
qutebrowser --target private-window
qbpm launch unsplash
qbpm launch mum
slack
steam
fin

app=$(echo "$apps" | dmenu -p gui)
[[ -n $app ]] || exit

exec $app & disown
