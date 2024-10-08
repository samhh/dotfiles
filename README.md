# dotfiles

Configuring the universe with [Nix](https://nixos.org).

The repo currently configures two hosts, a macOS development machine and a NixOS homelab.

## Usage

Build the Home Manager config for macOS or the NixOS config for the homelab. I manage the latter over SSH from the former.

```console
$ home-manager switch --flake .
# nixos-rebuild switch --flake ".#"
```

### Packages

Packages not in nixpkgs can be defined in `:/packages/`. These are available as flake outputs.

```console
$ nix run ".#foo" -- --bar
```

## History

I used to run various different operating systems and window managers. Those configs are preserved in this repo's commit history.

- NixOS/Sway/Neovim: `macos-redux~1`
- Arch/XMonad: `nixos~1`
- nix-darwin: `rm-darwin~1`
