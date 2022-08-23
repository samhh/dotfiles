{ ... }:

{
  imports = [
    ../../modules/nixos

    ../../modules/editor.nix
    ../../modules/terminal.nix
    ../../modules/vcs.nix

    ./audio.nix
    ./auth.nix
    ./backup.nix
    ./core.nix
    ./de.nix
    ./email.nix
    ./gaming.nix
    ./hardware.nix
    ./misc.nix
    ./network.nix
    ./user.nix
    ./web.nix
  ];
}
