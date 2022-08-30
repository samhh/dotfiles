{ config, pkgs, ... }:

{
  home-manager.users.${config.username}.home.packages = with pkgs; [
    pass
    ripgrep
    shellcheck
  ];
}
