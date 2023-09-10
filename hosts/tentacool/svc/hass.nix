{ config, pkgs, ... }:

let
  lanPort = 8123;
  mDNSPort = 5353;
  homekitBridgePort = 21064;

  backup = pkgs.writeShellScript "hass-backup" ''
    set -e

    tmp=$(mktemp)

    ${pkgs.podman}/bin/podman volume export hass > "$tmp"
    ${pkgs.gnutar}/bin/tar -f "$tmp" --wildcards --delete 'home-assistant_v2.*'

    mv "$tmp" ${config.nas.path}/backups/tentacool/hass.tar
  '';
in
{
  networking.firewall = {
    allowedTCPPorts = [ lanPort homekitBridgePort ];
    allowedUDPPorts = [ mDNSPort ];
  };

  virtualisation.oci-containers.containers.hass = {
    image = "ghcr.io/home-assistant/home-assistant:2023.9.0";
    volumes = [
      "hass:/config"
    ];
    extraOptions = [
      "--network=host"
    ];
    environment.TZ = "Europe/London";
  };

  systemd = {
    services."hass-backup" = {
      description = "HASS backup";
      wantedBy = [ "multi-user.target" ];
      requires = [ "podman-hass.service" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = backup;
      };
    };

    timers."hass-backup" = {
      description = "Run HASS backup";
      wantedBy = [ "timers.target" ];
      timerConfig.OnCalendar = "daily";
    };
  };
}
