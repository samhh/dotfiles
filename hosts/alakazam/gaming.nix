{ pkgs, selfpkgs, uname, ... }:

{
  programs.steam.enable = true;

  home-manager.users.${uname} = {
    home.sessionVariables.STEAM_EXTRA_COMPAT_TOOLS_PATHS = selfpkgs.proton-ge;

    home.packages = with pkgs; with selfpkgs; [
      mangohud
      polymc
      proton-ge
    ];
  };
}
