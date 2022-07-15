{ pkgs, uname, ... }:

{
  boot.kernelPackages = pkgs.linuxPackages_latest;

  home-manager.users.${uname}.home.stateVersion = "21.11";
}
