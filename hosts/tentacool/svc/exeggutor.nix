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
      serial.port = "/dev/ttyUSB0";
      frontend = true;
    };
  };
}
