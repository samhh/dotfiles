#!/bin/sh

read -r -d '' apps << fin
chromium
discord
firefox-developer-edition
hades
nheko
obsidian
pavucontrol
piper
simplescreenrecorder
slack
steam
telegram-desktop
vimb
zoom
fin

app=$(echo "$apps" | rofi -window-title gui -dmenu)
[[ -n $app ]] || exit

exec $app & disown

