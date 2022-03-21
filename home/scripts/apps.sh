#!/bin/bash

read -r -d '' apps << fin
chromium
discord
discover-overlay
firefox-devedition
obsidian
pavucontrol
piper
qutebrowser
qutebrowser-unsplash
simplescreenrecorder
slack
steam
zoom
fin

app=$(echo "$apps" | rofi -window-title gui -dmenu)
[[ -n $app ]] || exit

exec $app & disown

