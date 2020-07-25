#!/bin/sh

num_due=$(task due.before:tomorrow+86400 status:pending count)

if (( num_due == 0 )); then exit 1; fi

echo $num_due

