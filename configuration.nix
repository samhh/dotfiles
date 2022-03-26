{ pkgs, ... }:

let
  home-manager = builtins.fetchTarball {
    url = "https://github.com/nix-community/home-manager/archive/release-21.11.tar.gz";
    sha256 = "1ix6cknhlrwpawlakrsd3616rgy1calnds2h6wfqrv6cfdwsyzzc";
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

  nixpkgs.config.allowUnfree = true;
  system.stateVersion = "21.11";
}
