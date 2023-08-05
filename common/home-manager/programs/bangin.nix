{ config, lib, pkgs, ... }:

let cfg = config.programs.bangin;
in
with lib; {
  options.programs.bangin = {
    enable = mkEnableOption "bangin";

    bangs = mkOption {
      type = types.attrsOf types.str;
      default = { };
      example = literalExpression ''
        { "!foo,!bar" = "https://foo.bar/{{{s}}}" }
      '';
      description = ''
        Set of custom bangs. Syntax mirrors banglists.
      '';
    };

    lists = mkOption {
      type = types.listOf types.str;
      default = [ ];
      example = literalExpression ''
        [ "https://foo.bar/baz.bangs" ]
      '';
      description = ''
        List of remote banglists. In the same spirit as adlists these don't
        need to be versioned.
      '';
    };
  };

  config =
    let unlines = builtins.concatStringsSep "\n";
    in
    mkIf cfg.enable {
      home.packages = [ pkgs.bangin ];

      xdg.configFile = {
        "bangin/bangin.bangs" = mkIf (cfg.bangs != { }) {
          text =
            let unpairs = mapAttrsToList (k: v: k + " " + v);
            in unlines (unpairs cfg.bangs);
        };

        "bangin/bangin.lists" = mkIf (cfg.lists != [ ]) {
          text = unlines cfg.lists;
        };
      };
    };
}
