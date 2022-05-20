{ pkgs, uname, ... }:

{
  home-manager.users.${uname} = {
    xdg.configFile.aerc.source = ./cfg/aerc;

    home.packages = with pkgs; [
      aerc
      urlscan
    ];
  };
}
