{ config, ... }:

let
  port = 8981;
  user = "paperless";
in
{
  age.secrets.ponyta = {
    file = ../../../secrets/ponyta.age;
    owner = user;
  };

  networking.firewall.allowedTCPPorts = [ port ];

  services.paperless = {
    enable = true;
    inherit port user;
    # By default binds only to loopback.
    address = "0.0.0.0";
    passwordFile = config.age.secrets.ponyta.path;
    mediaDir = "${config.nas.path}/ponyta";
    extraConfig.PAPERLESS_FILENAME_FORMAT =
      "{owner_username}/{created_year}{created_month}{created_day}-{title}";
  };
}
