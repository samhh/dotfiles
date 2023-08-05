{ config, ... }:

{
  system.stateVersion = "22.05";
  home-manager.users.${config.username}.home.stateVersion = "22.05";
}
