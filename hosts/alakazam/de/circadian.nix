{ config, ... }:

{
  home-manager.users.${config.username} = {
    services.gammastep = {
      enable = true;
      inherit (config.location) latitude longitude;
    };
  };
}
