{ pkgs, ... }:

{
  amoxide = import ./amoxide.nix { inherit pkgs; };
}
