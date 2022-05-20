#!/usr/bin/env bash

# xargs trims whitespace
ccd1=$( sensors k10temp-pci-00c3 | grep Tccd1 | cut -d "+" -f2 | xargs )
ccd2=$( sensors k10temp-pci-00c3 | grep Tccd2 | cut -d "+" -f2 | xargs )

echo "$ccd1, $ccd2"
