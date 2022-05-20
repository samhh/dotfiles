{ pkgs, ... }:

{
  boot.kernelPackages = pkgs.linuxPackages_latest;

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  system.stateVersion = "21.11";
}
