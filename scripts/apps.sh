#!/usr/bin/env bash

read -r -d '' apps << fin
chromium
discord
discover-overlay
firefox
obsidian
pavucontrol
piper
qutebrowser
qutebrowser --target private-window
qbpm launch unsplash
qbpm launch mum
slack
steam
zoom
fin

app=$(echo "$apps" | bemenu -p gui)
[[ -n $app ]] || exit

exec $app & disown
