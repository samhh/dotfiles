{ config, pkgs, ... }:

let
  webPort = 8080;
  backup = pkgs.writeShellScript "exeggutor-backup" ''
    ${pkgs.gnutar}/bin/tar -c -C ${config.services.zigbee2mqtt.dataDir} --exclude ./log . > \
      ${config.nas.path}/archive/tentacool/exeggutor.tar
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
    services."exeggutor-backup" = {
      description = "Exeggutor backup";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = backup;
      };
    };

    timers."exeggutor-backup" = {
      description = "Run Exeggutor backup";
      wantedBy = [ "timers.target" ];
      timerConfig.OnCalendar = "daily";
    };
  };
}
