let
  alakazam = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIlaygHmHl1sO3ubaT2e0SpDklY7uusiG6Eev93UIX1o";
  tentacool = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH2TgCqWTnBiqaNYjFi1mFnhmhEG7Me+n3FFcck4IgTb";

in
{
  "ddns-token.age".publicKeys = [ tentacool ];
  "pihole-pass.age".publicKeys = [ tentacool ];
  "radicale-htpasswd.age".publicKeys = [ tentacool ];
}
