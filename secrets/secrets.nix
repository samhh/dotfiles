let
  alakazam = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIlaygHmHl1sO3ubaT2e0SpDklY7uusiG6Eev93UIX1o";
  tentacool = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH2TgCqWTnBiqaNYjFi1mFnhmhEG7Me+n3FFcck4IgTb";

in
{
  "b2-env.age".publicKeys = [ alakazam tentacool ];
  "ddns-token.age".publicKeys = [ tentacool ];
  "gmail.age".publicKeys = [ alakazam ];
  "irc-token.age".publicKeys = [ alakazam ];
  "krabby.age".publicKeys = [ alakazam ];
  "migadu.age".publicKeys = [ alakazam tentacool ];
  "pihole-env.age".publicKeys = [ tentacool ];
  "radarr-api-key.age".publicKeys = [ tentacool ];
  "radarr-host.age".publicKeys = [ tentacool ];
  "radicale-htpasswd.age".publicKeys = [ tentacool ];
  "restic.age".publicKeys = [ alakazam tentacool ];
  "sonarr-api-key.age".publicKeys = [ tentacool ];
  "sonarr-host.age".publicKeys = [ tentacool ];
}
