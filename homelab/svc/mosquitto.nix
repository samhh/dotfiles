{ ... }:

let
  brokerPort = 1883;
in
{
  networking.firewall.allowedTCPPorts = [ brokerPort ];

  services.mosquitto = {
    enable = true;
    listeners = [
      # LAN w/o credentials
      {
        port = brokerPort;
        omitPasswordAuth = true;
        settings.allow_anonymous = true;
        acl = [ "topic readwrite #" ];
      }
    ];
  };
}
