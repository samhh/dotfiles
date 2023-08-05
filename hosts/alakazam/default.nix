{ nix-colors, ... }:

{
  imports = [
    nix-colors.homeManagerModules.default

    ./audio.nix
    ./auth.nix
    ./core.nix
    ./de
    ./editor.nix
    ./email.nix
    ./gaming.nix
    ./hardware.nix
    ./misc.nix
    ./network.nix
    ./secrets.nix
    ./security.nix
    ./shell.nix
    ./terminal.nix
    ./user.nix
    ./vcs.nix
    ./web.nix
  ];
}
