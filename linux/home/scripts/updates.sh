#!/bin/sh

updates_arch=$(checkupdates | wc -l)
updates_aur=$(checkupdates-aur | wc -l)

echo $(( "$updates_arch" + "$updates_aur" ))

