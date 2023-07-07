{ config, pkgs, ... }:

let
  lanPort = 8123;
  mDNSPort = 5353;
  homekitBridgePort = 21064;

  backup = pkgs.writeShellScript "starmie-backup" ''
    set -e

    tmp=$(mktemp)

    ${pkgs.podman}/bin/podman volume export hass > "$tmp"
    ${pkgs.gnutar}/bin/tar -f "$tmp" --wildcards --delete 'home-assistant_v2.*'

    mv "$tmp" ${config.nas.path}/archive/tentacool/starmie.tar
  '';
in
{
  networking.firewall = {
    allowedTCPPorts = [ lanPort homekitBridgePort ];
    allowedUDPPorts = [ mDNSPort ];
  };

  virtualisation.oci-containers.containers.hass = {
    image = "ghcr.io/home-assistant/home-assistant:2023.7.1";
    volumes = [
      "hass:/config"
    ];
    extraOptions = [
      "--network=host"
    ];
    environment.TZ = "Europe/London";
  };

  systemd = {
    services."starmie-backup" = {
      description = "Starmie backup";
      wantedBy = [ "multi-user.target" ];
      requires = [ "podman-hass.service" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = backup;
      };
    };

    timers."starmie-backup" = {
      description = "Run Starmie backup";
      wantedBy = [ "timers.target" ];
      timerConfig.OnCalendar = "daily";
    };
  };
}
