{ ... }:

{
  imports = [
    ../../modules/nixos
    ../../modules/headful

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
