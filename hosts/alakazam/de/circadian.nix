{ config, pkgs, ... }:

{
  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [ darkman ];
  };

  home-manager.users.${config.username} = {
    services.gammastep = {
      enable = true;
      inherit (config.location) latitude longitude;
    };

    xdg.configFile."darkman/config.yaml".text = ''
      lat: ${toString config.location.latitude}
      lng: ${toString config.location.longitude}
    '';

    home.packages = with pkgs; [ darkman ];
  };
}
