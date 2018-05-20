#!/bin/sh

# Start ssh-agent
eval $(ssh-agent)

# Import colour scheme from X resources
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

# Disable screensaver & DPMS
xset s off -dpms

# Lock immediately after awaking from sleep
# Note that xss-lock hooks into systemd, so just call "systemctl suspend" whenever you want to sleep/lock
xss-lock -- i3lock-next &

# Notifications
dunst &

# Screen colour temperature
redshift-gtk &

# Compositor (transparency & misc)
compton -b -c --inactive-dim .1 --backend glx --vsync drm

# Set transparency dynamically on entire windows
devilspie -a &

# WM
exec i3
