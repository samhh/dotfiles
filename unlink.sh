#!/bin/sh

# Set PWD to location of script (i.e. dotfiles repo root)
cd "$(dirname "$0")"

stow -D root -t /
stow -D home -t $HOME

