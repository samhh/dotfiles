{ ... }:

let brokerPort = 1883;
in
{
  networking.firewall.allowedTCPPorts = [
    brokerPort
  ];

  services.mosquitto = {
    enable = true;
    listeners = [
      # LAN w/o credentials
      {
        address = "0.0.0.0";
        port = brokerPort;
        settings.allow_anonymous = true;
        acl = [
          "topic readwrite #"
        ];
      }
    ];
  };
}
