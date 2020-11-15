#!/bin/sh

n=$(ipptool -tv ipp://BRW7440BBBDE8DC/BINARY_P1 get-printer-attributes.test | grep marker-levels | cut -d = -f 2 | xargs)

echo "Printer toner: $n% remaining"

