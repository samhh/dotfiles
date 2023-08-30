{ config, pkgs, ... }:

let port = 1234;
in
{
  networking.firewall.allowedTCPPorts = [ port ];

  services.bangin-server-node = {
    enable = true;
    inherit port;
    fallback = "https://kagi.com/search?q={{{s}}}";
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
