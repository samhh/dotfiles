{ ... }:

let mosquittoPort = 1883;
in
{
  networking.firewall.allowedTCPPorts = [
    # Mosquitto broker
    mosquittoPort

    # Zigbee2MQTT frontend
    8080
  ];

  services.mosquitto = {
    enable = true;
    listeners = [
      # LAN w/o credentials
      {
        address = "0.0.0.0";
        port = mosquittoPort;
        settings.allow_anonymous = true;
        acl = [
          "topic readwrite #"
        ];
      }
    ];
  };

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
