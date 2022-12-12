{ config, pkgs, ... }:

let port = 1234;
in
{
  services.nginx.virtualHosts."tauros.samhh.com" = {
    enableACME = true;
    forceSSL = true;
    locations."/".proxyPass = "http://127.0.0.1:${toString port}";
  };

  services.ddclient.domains = [ "tauros" ];

  services.bangin-server-node = {
    enable = true;
    port = port;
  };

  home-manager.users.${config.username} = {
    programs.bangin = {
      enable = true;
      lists =
        let f = x: "https://git.sr.ht/~samhh/${x}.bangs/blob/master/${x}.bangs";
        in
        map f [
          "arch"
          "dev"
          "english"
          "italiano"
          "nix"
          "pcgaming"
          "prelude"
          "uk"
        ];
    };

    home.packages = with pkgs; [ bangup ];
  };
}
