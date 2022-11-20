{ pkgs, ... }:

{
  bangin = pkgs.callPackage ./bangin.nix { };
  bangin-server-node = pkgs.callPackage ./bangin-server-node.nix { };
  bangup = pkgs.callPackage ./bangup { };
  corrupter = pkgs.callPackage ./corrupter.nix { };
  proton-ge = pkgs.callPackage ./proton-ge.nix { };
  qbpm = pkgs.callPackage ./qbpm.nix { };
  tshm = pkgs.callPackage ./tshm.nix { };
}
