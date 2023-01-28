{ config, pkgs, ... }:

let
  dnsPort = 53;
  webPort = 8053;
  backup = pkgs.writeShellScript "onix-backup" ''
    ${pkgs.podman}/bin/podman exec pihole sh -c \
      'pihole -a -t && mv /pi-hole-tentacool-teleporter_*.tar.gz /backup.tar.gz'

    ${pkgs.podman}/bin/podman cp pihole:/backup.tar.gz ${config.nas.path}/archive/tentacool/onix.tar.gz
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
    image = "pihole/pihole:2022.11.2";
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
