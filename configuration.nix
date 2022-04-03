{ pkgs, ... }:

let
  # From 20220106. Close to 21.11, but supports the newly added swayidle module.
  home-manager = builtins.fetchTarball {
    url = "https://github.com/nix-community/home-manager/archive/5fb55d51e2dcdc0dd5f6c82968c3edff00d73b2b.tar.gz";
    sha256 = "0mpl570j1yawkh60bqrygwzkmaz51z6rx5syikrjnr4qz9ibj5g5";
  };
in {
  imports =
    [
      (import "${home-manager}/nixos")
      ./audio.nix
      ./backup.nix
      ./de.nix
      ./editor.nix
      ./email.nix
      ./gaming.nix
      ./hardware.nix
      ./misc.nix
      ./network.nix
      ./rss.nix
      ./security.nix
      ./terminal.nix
      ./user.nix
      ./vcs.nix
      ./web.nix
    ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
  nixpkgs.config.allowUnfree = true;
  system.stateVersion = "21.11";
}
