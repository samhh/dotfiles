#!/bin/sh

/etc/X11/xinit/xinitrc.d/50-systemd-user.sh
xrdb -merge ~/.Xresources
exec xmonad

