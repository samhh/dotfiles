{ config, lib, pkgs, ... }:

let cfg = config.services.bangin-server-node;
in
with lib; {
  options.services.bangin-server-node = {
    enable = mkEnableOption "bangin-server-node";

    port = mkOption {
      type = types.int;
    };
  };

  config = mkIf cfg.enable {
    systemd.services.bangin-server-node = {
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        # So that bangin has access to its config.
        User = config.username;
        ExecStart =
          "${pkgs.bangin-server-node}/bin/bangin-server-node ${toString cfg.port}";
      };
    };
  };
}
