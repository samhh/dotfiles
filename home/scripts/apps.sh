#!/bin/sh

read -r -d '' apps << fin
discord
element-desktop
firefox-developer-edition
hades
pavucontrol
piper
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

