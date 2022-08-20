#!/bin/sh

img="/tmp/lock.png"

grimshot save screen - | corrupter - > "$img"
swaylock -i "$img"
