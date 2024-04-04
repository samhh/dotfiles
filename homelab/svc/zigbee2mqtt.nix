{ ... }:

let
  webPort = 8080;
in
{
  networking.firewall.allowedTCPPorts = [ webPort ];

  services.zigbee2mqtt = {
    enable = true;
    settings = {
      homeassistant = true;
      permit_join = false;
      serial.port = "/dev/serial/by-id/usb-ITead_Sonoff_Zigbee_3.0_USB_Dongle_Plus_803e9325733bec11987b9e957a0af07f-if00-port0";
      frontend = true;
    };
  };
}
