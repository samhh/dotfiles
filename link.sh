#!/bin/sh

# Set PWD to location of script (i.e. dotfiles repo root)
cd "$(dirname "$0")"

# Establish which OS we're working on
if [ `uname` == 'Linux' ]; then
  osdir="linux"
elif [ `uname` == 'Darwin' ]; then
  osdir="macos"
else
  exit 1
fi

stow -R --adopt home -d $osdir -t $HOME
stow -R --adopt home -d shared -t $HOME

