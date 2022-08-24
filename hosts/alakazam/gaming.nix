{ pkgs, uname, ... }:

let
  proton-ge-custom = pkgs.callPackage ../../pkg/proton-ge.nix { };
in
{
  programs.steam.enable = true;

  home-manager.users.${uname} = {
    home.sessionVariables.STEAM_EXTRA_COMPAT_TOOLS_PATHS = proton-ge-custom;

    home.packages = with pkgs; [
      mangohud
      polymc
      proton-ge-custom
    ];
  };
}
