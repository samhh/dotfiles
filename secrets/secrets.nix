let
  tentacool = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH2TgCqWTnBiqaNYjFi1mFnhmhEG7Me+n3FFcck4IgTb";
in
{
  "b2-env.age".publicKeys = [ tentacool ];
  "radarr-api-key.age".publicKeys = [ tentacool ];
  "radarr-host.age".publicKeys = [ tentacool ];
  "restic.age".publicKeys = [ tentacool ];
  "sonarr-api-key.age".publicKeys = [ tentacool ];
  "sonarr-host.age".publicKeys = [ tentacool ];
}
