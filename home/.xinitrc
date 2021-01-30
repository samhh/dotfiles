#!/bin/sh

/etc/X11/xinit/xinitrc.d/50-systemd-user.sh
systemctl --user start xorg.target
xrdb -merge ~/.Xresources
setxkbmap -option compose:ralt
exec xmonad

