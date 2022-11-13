{ config, pkgs, ... }:

{
  programs.steam.enable = true;

  home-manager.users.${config.username} = {
    home.sessionVariables.STEAM_EXTRA_COMPAT_TOOLS_PATHS = pkgs.proton-ge;

    home.packages = with pkgs; [
      lutris-free
      mangohud
      prismlauncher
      proton-ge
    ];
  };
}
