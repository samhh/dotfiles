{ pkgs, ... }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    agenix
    git
    git-crypt
    nixpkgs-fmt
  ];
}
