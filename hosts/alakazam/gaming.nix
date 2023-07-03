{ config, lib, pkgs, ... }:

{
  programs.steam.enable = true;

  home-manager.users.${config.username} = {
    home.sessionVariables.STEAM_EXTRA_COMPAT_TOOLS_PATHS = pkgs.proton-ge;

    programs.mangohud = {
      enable = true;
      settings =
        let
          # This is additive atop the defaults.
          sensors = [ "cpu_temp" "gpu_temp" ];
          f = x: { name = x; value = true; };
        in
        lib.listToAttrs (map f sensors);
    };

    home.packages = with pkgs; [
      gamescope
      lutris-free
      prismlauncher
      proton-ge
    ];
  };
}
