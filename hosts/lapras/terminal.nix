{ config, pkgs, uname, ... }:

{
  home-manager.users.${uname}.programs.fish = {
    shellInit = ''
      set fish_function_path $fish_function_path ${pkgs.fishPlugins.foreign-env}/share/fish/vendor_functions.d
      fenv source ${config.system.build.setEnvironment}
    '';

    shellAbbrs = {
      "up" = "darwin-rebuild build";
      "upp" = "sudo darwin-rebuild switch";
    };
  };
}
