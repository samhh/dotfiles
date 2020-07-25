# dotfiles

Here are my dotfiles, separated by branch for four different machines:

- desktop: Arch, xmonad
- laptop: Arch, Sway
- work: macOS
- controller: Raspberry Pi OS

I spend most of my personal time on the desktop, however my work machine is stuck with macOS. As such, I've tried to make working with both of these simultaneously as seamless as possible. For example, my experience in the terminal and the editor should be close to identical on each OS.

My desktop runs xmonad as my Nvidia video card isn't Sway-compatible, and I love the flexibility of defining its behaviour in Haskell. My laptop continues to run Sway as it's tear-free and I don't feel compelled to change it when it's stable and working well.

The "controller" is a Raspberry Pi that's sitting out of the way somewhere. It handles home automation tasks.

I've tried to utilise the [Nord](https://www.nordtheme.com) colour scheme where possible.

## Symlinking

A shell script (`./link.sh`) is included that sets up symlinks for everything. This uses Stow under the hood.

Any configs that belong outside of the user home directory or require user input due to user secrets are placed in the `manual` directory and must be manually managed.

There are plenty of implicit software prerequisites as these are the configs I use every day down to a byte.

