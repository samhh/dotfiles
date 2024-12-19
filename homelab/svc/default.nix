{ ... }:

{
  imports = [
    ./hass.nix
    ./matterbridge.nix
    ./mosquitto.nix
    ./zigbee2mqtt.nix
  ];
}
