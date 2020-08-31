#!/bin/sh

export SOCKS_SERVER="127.0.0.1:1"

content=$(socksify w3m -T text/html -cols $(tput cols) -dump -o display_image=false -o display_link_number=true)
url=$(echo "$content" | sed -n '/References:/,$ p' | tail -n +3 | cut -d ' ' -f 2 | sed -n "$1 p")
exec xdg-open $url

