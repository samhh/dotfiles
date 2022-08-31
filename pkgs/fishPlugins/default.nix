{ pkgs, ... }:

{
  fish-minimal-theme = pkgs.callPackage ./fish-minimal-theme.nix { };
  z = pkgs.callPackage ./z.nix { };
}
