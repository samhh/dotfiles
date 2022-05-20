#!/bin/sh

level=$(cat /sys/class/power_supply/hidpp_battery_*/capacity_level)
echo "Mouse charge status: $level"
