#!/bin/sh

# Set PWD to location of script (i.e. dotfiles repo root)
cd "$(dirname "$0")"

# Establish which OS we're working on
if [ `uname` == 'Linux' ]; then
  osdir="linux"

  # Ensure we are run as root
  if ! [ $EUID -eq 0 ]; then
    echo "ERROR: This script must be run as root."
    exit 126
  fi

  stow -R --adopt root -d $osdir -t /

elif [ `uname` == 'Darwin' ]; then
  osdir="macos"
else
  exit 1
fi

stow -R --adopt home -d $osdir -t $HOME
stow -R --adopt home -d shared -t $HOME

