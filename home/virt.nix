# Colima must be started manually on boot:
# $ colima start

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    colima
    docker
  ];
}
