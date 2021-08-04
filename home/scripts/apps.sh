#!/bin/bash

read -r -d '' apps << fin
chromium
discord
firefox-developer-edition
hades
nheko
obsidian
pavucontrol
piper
qutebrowser
simplescreenrecorder
slack
steam
telegram-desktop
fin

app=$(echo "$apps" | rofi -window-title gui -dmenu)
[[ -n $app ]] || exit

exec $app & disown

