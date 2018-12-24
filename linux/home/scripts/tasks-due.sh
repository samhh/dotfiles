#!/bin/sh

num_due=$(task due.before:tomorrow+86400 status:pending count)

echo $num_due

