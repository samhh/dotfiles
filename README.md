# dotfiles

Configuring the universe.

## Usage

Build the Home Manager config for macOS.

```console
$ home-manager switch --flake path/to/repo
```

Many packages are installed with Homebrew instead as for my needs the UX is better. A Brewfile is periodically dumped for unversioned redundancy. Homebrew itself is installed manually, like Nix.

```console
$ brew bundle dump -f --file path/to/repo/Brewfile
```

## History

Over the years I've run various different operating systems, window managers, and editors. Those configs are preserved in this repo's commit history:

- NixOS/Home Assistant: `rm-homelab~1`
- Helix: `zed~1`
- NixOS/Sway/Neovim: `macos-redux~1`
- Arch/XMonad: `nixos~1`
- nix-darwin: `rm-darwin~1`
