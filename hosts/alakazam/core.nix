{ pkgs, ... }:

{
  system.stateVersion = "22.05";

  boot.kernelPackages = pkgs.linuxPackages_latest;
}
