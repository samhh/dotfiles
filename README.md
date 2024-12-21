# dotfiles

Configuring the universe with [Nix](https://nixos.org).

The repo currently configures two hosts, a macOS development machine and a NixOS homelab.

## Usage

Build the Home Manager config for macOS or the NixOS config for the homelab. I manage the latter over SSH from the former.

```console
$ home-manager switch --flake path/to/repo
# nixos-rebuild switch --flake 'path/to/repo#'
```

### Packages

Packages not in nixpkgs can be defined in `:/packages/`. These are available as flake outputs.

```console
$ nix run 'path/to/repo#foo' -- --bar
```

## History

Over the years I've run various different operating systems, window managers, and editors. Those configs are preserved in this repo's commit history:

- Helix: `zed~1`
- NixOS/Sway/Neovim: `macos-redux~1`
- Arch/XMonad: `nixos~1`
- nix-darwin: `rm-darwin~1`
