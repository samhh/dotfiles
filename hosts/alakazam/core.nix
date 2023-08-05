{ config, pkgs, ... }:

{
  system.stateVersion = "22.05";
  home-manager.users.${config.username}.home.stateVersion = "22.05";

  boot.kernelPackages = pkgs.linuxPackages_latest;

  environment.systemPackages = with pkgs; [ deploy-rs ];
}
