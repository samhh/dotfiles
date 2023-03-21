#!/usr/bin/env bash

read -r -d '' apps << fin
1password
chromium
cider
emote
ffplay /dev/video0
firefox
gddccontrol
librewolf
librewolf -P unsplash
librewolf --private-window
lutris
obsidian
pavucontrol
piper
prismlauncher
slack
steam
fin

app=$(echo "$apps" | tofi --prompt gui)
[[ -n $app ]] || exit

exec $app & disown
