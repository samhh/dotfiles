{ config, pkgs, ... }:

{
  home.stateVersion = "22.05";

  home.username = "sam";
  home.homeDirectory = "/home/sam";
  xdg.cacheHome = "/home/sam/.cache/";
  xdg.configHome = "/home/sam/.config/";
  xdg.dataHome = "/home/sam/.local/share/";

  programs.home-manager.enable = true;

  home.packages = [
    pkgs.direnv
  ];

  services.lorri.enable = true;
}
