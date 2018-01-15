#!/bin/sh

updates_arch=$(checkupdates | wc -l)
updates_aur=$(pacaur -k | wc -l)

echo $(( "$updates_arch" + "$updates_aur" ))

