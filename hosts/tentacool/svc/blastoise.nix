{ nasPath, ... }:

{
  services.navidrome = {
    enable = true;
    settings = {
      MusicFolder = nasPath + "/music/archive";
      ScanSchedule = "@daily";
      EnableTranscodingConfig = true;
    };
  };

  services.nginx.virtualHosts."blastoise.samhh.com" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:4533";
    };
  };

  services.ddclient.domains = [ "blastoise" ];
}
