{ pkgs, ... }:

{
  fish-minimal-theme = pkgs.callPackage ./fish-minimal-theme.nix { };
}
