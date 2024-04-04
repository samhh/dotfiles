{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.programs.efm-langserver;

  toolType = types.submodule {
    options = {
      root-markers = mkOption { type = types.listOf types.str; };

      format-command = mkOption { type = types.str; };

      format-stdin = mkOption { type = types.bool; };
    };
  };

  mkCfg =
    languages:
    generators.toYAML { } {
      version = 2;
      lint-debounce = "1s";
      inherit languages;
    };
in
{
  options.programs.efm-langserver = {
    enable = mkEnableOption "efm-langserver";

    languages = mkOption { type = types.attrsOf (types.listOf toolType); };
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.efm-langserver ];

    xdg.configFile."efm-langserver/config.yaml".text = mkCfg cfg.languages;
  };
}
