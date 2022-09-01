{ ... }:

{
  imports = [
    ../../headful

    ./auth.nix
    ./core.nix
    ./misc.nix
    ./terminal.nix
  ];
}
