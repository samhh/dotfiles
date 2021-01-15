#!/bin/sh

newsboat -x reload
echo $(newsboat -x print-unread | cut -d ' ' -f 1)

