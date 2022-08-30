{ config, pkgs, ... }:

{
  boot.kernelPackages = pkgs.linuxPackages_latest;

  home-manager.users.${config.username}.home.stateVersion = "22.05";
}
