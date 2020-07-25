#!/bin/bash

random_selected=$(ls ~/pics/bg/* | xargs shuf -n1 -e)

killall swaybg 2> /dev/null
swaybg -i $random_selected -m fill

