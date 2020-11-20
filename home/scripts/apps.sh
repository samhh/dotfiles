#!/bin/sh

read -r -d '' apps << fin
android-studio
discord
element-desktop
factorio
firefox-developer-edition
fm20
hades
multimc
nvidia-settings
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

