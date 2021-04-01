#!/bin/sh

read -r -d '' apps << fin
chromium
discord
element-desktop
firefox-developer-edition
hades
obsidian
pavucontrol
piper
protonmail-import-export-app
slack
steam
telegram-desktop
vimb
vimb-work
zoom
fin

app=$(echo "$apps" | rofi -window-title gui -dmenu)
[[ -n $app ]] || exit

exec $app & disown

