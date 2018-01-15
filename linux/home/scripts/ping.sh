#!/bin/bash

ping 8.8.8.8 -c 1 | awk -F" |=" '/time=/{printf "%i%s\n", $10, "ms"}'

