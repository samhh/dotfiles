{ pkgs, ... }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    agenix
    deadnix
    nixpkgs-fmt
    nushell
  ];
}
