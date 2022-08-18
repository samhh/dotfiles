{ ... }:

{
  networking.firewall.allowedTCPPorts = [
    # Zigbee2MQTT frontend
    8080
  ];

  services.mosquitto = {
    enable = true;
    # Implies local-only mode, which works out of the box where for some reason
    # an `allow_anonymous` listener doesn't.
    listeners = [ ];
  };

  services.zigbee2mqtt = {
    enable = true;
    settings = {
      homeassistant = true;
      permit_join = true;
      serial.port = "/dev/ttyUSB0";
      frontend = true;
    };
  };
}
