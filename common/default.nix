{ ... }:

{
  imports = [
    ./services
    ./backup.nix
    ./config.nix
    ./core.nix
    ./locale.nix
    ./secrets.nix
    ./user.nix
  ];
}
