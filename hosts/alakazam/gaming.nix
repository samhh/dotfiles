{ pkgs, uname, ... }:

{
  programs.steam.enable = true;

  home-manager.users.${uname} = {
    home.sessionVariables.STEAM_EXTRA_COMPAT_TOOLS_PATHS = pkgs.proton-ge;

    home.packages = with pkgs; [
      mangohud
      polymc
      proton-ge
    ];
  };
}
