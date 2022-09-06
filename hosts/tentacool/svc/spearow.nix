{ ... }:

let mosquittoPort = 1883;
in
{
  networking.firewall.allowedTCPPorts = [
    # Mosquitto broker
    mosquittoPort
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
}
