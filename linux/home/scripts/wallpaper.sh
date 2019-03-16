#!/bin/bash

random_selected=$(ls ~/pics/bg/* | xargs shuf -n1 -e)

swaymsg "output * bg $random_selected fill"

