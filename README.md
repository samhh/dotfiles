# dotfiles

My personal dotfiles and miscellaneous configs.

## Overview

I spend most of my personal time on the Arch partition of my MBP, however my work laptop is stuck with macOS. As such, I've tried to make working with both of these simultaneously as seemless as possible. For example, my experience in the terminal and the editor should be close to identical on each OS.

As of time of writing, most of the Linux configs adhere to the [Gruvbox Dark](https://github.com/morhetz/gruvbox) color scheme for visual cohesion.

## Project Structure

At its core, everything is split up into five basic dirs:

- **Linux**
- **macOS**
- **Shared**: Configs that should work out of the box on both Linux and macOS.
- **NAS**: Configs for my home NAS (running off a Raspberry Pi).
- **Misc**: At the moment this is exclusively a CS:GO config (that I happen to use on Windows).

You can use the included `./link.sh` helper on Linux/macOS to automatically set up syslinks for all configs. This uses Stow under the hood.

Any configs that require user input due to user secrets (e.g. GPG key in Git config) are placed in appended *_secret* dirs, separately from the clean configs. You will need to manually manage these.

There are plenty of software prerequisites as these are the configs I use every day down to a byte. Read the files you're downloading to understand what they are and what might be required for them to work.

## Caveats

Note that under Linux there are presently configs for three different terminals, all of which suffer different defects. Pick whichever one suits you best. For reference, here are said terminals and accompanying defects:

- Alacritty: No scrollback, no font ligatures.
- Kitty: Inconsistent scrollback behaviour, sometimes fails to gracefully exit.
- Termite: No font ligatures, supposedly comparatively poor performance.
