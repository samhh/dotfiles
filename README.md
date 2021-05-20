# dotfiles

Here are my dotfiles targeting a machine running xmonad on Arch. Colour schemes are generated on the fly with [pywal](https://github.com/dylanaraps/pywal).

## Symlinking

A shell script (`./link.sh`) is included that sets up symlinks for everything. This uses Stow under the hood.

Any configs that belong outside of the user home directory or require user input due to user secrets are placed in the `manual` directory and must be manually managed.

There are plenty of implicit software prerequisites as these are the configs I use every day down to a byte.

