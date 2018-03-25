#!/bin/sh

updates_arch=$(checkupdates | wc -l)
#updates_aur=$(pacaur -k | wc -l)
updates_aur=0

echo $(( "$updates_arch" + "$updates_aur" ))

