#!/bin/bash

read -r -d '' apps << fin
chromium
discord
discover-overlay
firefox-developer-edition
nheko
obsidian
pavucontrol
piper
qutebrowser
qutebrowser-unsplash
simplescreenrecorder
slack
steam
telegram-desktop
zoom
fin

app=$(echo "$apps" | rofi -window-title gui -dmenu)
[[ -n $app ]] || exit

exec $app & disown

