#!/usr/bin/env bash

running=$(docker ps --format "{{.Names}}" | wc -l)
total=$(docker ps -a --format "{{.Names}}" | wc -l)

echo "$running/$total"

