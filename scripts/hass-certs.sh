#!/bin/sh

doas certbot certonly --standalone -n -d home.samhh.com \
  --agree-tos -m hello@samhh.com
