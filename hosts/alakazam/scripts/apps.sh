#!/usr/bin/env bash

read -r -d '' apps << fin
1password
chromium
cider
emote
firefox
gddccontrol
lutris
obsidian
pavucontrol
piper
prismlauncher
qutebrowser
qutebrowser --target private-window
qbpm launch unsplash
slack
steam
fin

app=$(echo "$apps" | tofi --prompt gui)
[[ -n $app ]] || exit

exec $app & disown
