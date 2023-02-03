{ config, pkgs, ... }:

let
  dnsPort = 53;
  webPort = 8053;
  backup = pkgs.writeShellScript "onix-backup" ''
    set -e

    tmp=$(mktemp)

    ${pkgs.podman}/bin/podman exec pihole sh -c \
      'pihole -a -t && mv /pi-hole-tentacool-teleporter_*.tar.gz /backup.tar.gz'

    ${pkgs.podman}/bin/podman cp pihole:/backup.tar.gz "$tmp"

    mv "$tmp" ${config.nas.path}/archive/tentacool/onix.tar.gz
  '';
in
{
  networking.firewall.allowedTCPPorts = [
    dnsPort
    webPort
  ];
  networking.firewall.allowedUDPPorts = [
    dnsPort
  ];

  virtualisation.oci-containers.containers.pihole = {
    image = "pihole/pihole:2023.01.10";
    volumes = [
      "pihole:/etc/pihole"
      "dnsmasq:/etc/dnsmasq.d"
    ];
    extraOptions = [
      "--network=host"
    ];
    environment = {
      WEB_PORT = toString webPort;
      TZ = "Europe/London";
    };
    environmentFiles = [
      config.age.secrets.pihole-env.path
    ];
  };

  systemd = {
    services."onix-backup" = {
      description = "Onix backup";
      wantedBy = [ "multi-user.target" ];
      requires = [ "podman-pihole.service" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = backup;
      };
    };

    timers."onix-backup" = {
      description = "Run Onix backup";
      wantedBy = [ "timers.target" ];
      timerConfig.OnCalendar = "daily";
    };
  };
}
