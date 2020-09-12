#!/bin/sh

read -r -d '' apps << fin
baba-is-you
discord
element-desktop
factorio
firefox-developer-edition
fm20
multimc
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

