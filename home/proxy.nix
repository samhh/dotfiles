# Caddy must be started manually on boot:
# $ caddy start -c path/to/repo/home/proxy/Caddyfile

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    caddy
  ];
}
