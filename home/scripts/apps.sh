#!/bin/sh

read -r -d '' apps << fin
discord
element-desktop
factorio
firefox-developer-edition
fm20
nvidia-settings
piper
slack
steam
telegram-desktop
vimb
fin

app=$(echo "$apps" | rofi -window-title gui -dmenu)
[[ -n $app ]] || exit

exec $app & disown

