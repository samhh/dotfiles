# dotfiles

My personal dotfiles and miscellaneous configs.

## Overview

I spend most of my personal time on the Arch partition of my MBP, however my work laptop is stuck with macOS. As such, I've tried to make working with both of these simultaneously as seemless as possible. For example, my experience in the terminal and the editor should be close to identical on each OS.

As of time of writing, most of the Linux configs adhere to the [Gruvbox Dark](https://github.com/morhetz/gruvbox) color scheme for visual cohesion.

## Project Structure

Everything is split up into four dirs:

- **Linux**
- **macOS**
- **Shared**: Stuff that should work out of the box on both Linux and macOS.
- **Misc**: At the moment this is exclusively a CS:GO config (that I happen to use on Windows).

There are plenty of software prerequisites as these are the configs I use every day down to a byte. Read the files you're downloading to understand what they are and what might be required for them to work.

## Caveats

Note that under Linux there are presently configs for three different terminals, all of which suffer different defects. Pick whichever one suits you best. For reference, here are said terminals and accompanying defects:

- Alacritty: No scrollback, no font ligatures.
- Kitty: Weird visual bug with the cursor when the window is inactive.
- Termite: No font ligatures, supposedly comparatively poor performance.
