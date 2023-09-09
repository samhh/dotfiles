{ pkgs, ... }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    agenix
    deadnix
    git-crypt
    nixpkgs-fmt
  ];
}
