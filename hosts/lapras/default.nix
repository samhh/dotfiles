{ ... }:

{
  imports = [
    ../../modules/headful

    ./auth.nix
    ./core.nix
    ./misc.nix
  ];
}