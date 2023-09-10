{ config, pkgs, ... }:

let
  webPort = 8080;
  backup = pkgs.writeShellScript "zigbee2mqtt-backup" ''
    set -e

    tmp=$(mktemp)

    ${pkgs.gnutar}/bin/tar -c -C ${config.services.zigbee2mqtt.dataDir} --exclude ./log . > "$tmp"

    mv "$tmp" ${config.nas.path}/archive/tentacool/zigbee2mqtt.tar
  '';
in
{
  networking.firewall.allowedTCPPorts = [
    webPort
  ];

  services.zigbee2mqtt = {
    enable = true;
    settings = {
      homeassistant = true;
      permit_join = false;
      serial.port = "/dev/serial/by-id/usb-ITead_Sonoff_Zigbee_3.0_USB_Dongle_Plus_803e9325733bec11987b9e957a0af07f-if00-port0";
      frontend = true;
    };
  };

  systemd = {
    services."zigbee2mqtt-backup" = {
      description = "zigbee2mqtt backup";
      wantedBy = [ "multi-user.target" ];
      requires = [ "zigbee2mqtt.service" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = backup;
      };
    };

    timers."zigbee2mqtt-backup" = {
      description = "Run zigbee2mqtt backup";
      wantedBy = [ "timers.target" ];
      timerConfig.OnCalendar = "daily";
    };
  };
}
