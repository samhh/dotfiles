{ config, lib, pkgs, tshm-plugin, ... }:

with lib;
let
  cfg = config.programs.tshm;
in
{
  options.programs.tshm = {
    enable = mkEnableOption "tshm";

    installEditorPlugin = mkOption {
      type = types.bool;
      default = false;
      description = "Installs the compiler plugin to `$XDG_DATA_HOME/npmlibs/node_modules/typescript-tshm-plugin`. This path can be provided to language server configurations.";
    };
  };

  config =
    mkIf cfg.enable {
      home.packages = [ pkgs.tshm ];

      xdg.dataFile."npmlibs/node_modules/typescript-tshm-plugin".source = tshm-plugin;
    };
}
