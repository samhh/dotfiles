#!/bin/sh

apps="1password
chromium
emote
ffplay /dev/video0
firefox
gddccontrol
librewolf
librewolf --private-window
lutris
obsidian obsidian://open?vault=vault
obsidian obsidian://open?vault=docs
pavucontrol
piper
prismlauncher
slack
steam"

app=$(echo "$apps" | tofi --prompt gui)
[ -n "$app" ] || exit

exec $app &
