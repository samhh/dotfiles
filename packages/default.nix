{ pkgs, ... }:

{
  bangin = pkgs.callPackage ./bangin.nix { };
  bangin-server-node = pkgs.callPackage ./bangin-server-node.nix { };
  bangup = pkgs.callPackage ./bangup { };
  tshm = pkgs.callPackage ./tshm.nix { };
}
