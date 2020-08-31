#!/bin/bash

perc=$(cat /sys/class/power_supply/sony_controller_battery_*/capacity)
echo "DS4 charge: ~$perc% remaining"

