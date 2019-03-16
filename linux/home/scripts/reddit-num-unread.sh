#!/usr/bin/sh

feed=$1

if [[ -z $feed ]]; then exit 1; fi

# reddit api returns rate limit error for curl user agent
private_feed="https://www.reddit.com/message/unread/.json?feed=$feed"
user_agent='random'

num_msgs=$(curl -s $private_feed -A $user_agent | jq -r '.data.dist')

if (( num_msgs == 0 )); then exit 1; fi

echo $num_msgs

