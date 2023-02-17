{ pkgs, ... }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    agenix
    git
    git-crypt
    nixpkgs-fmt
  ];
}
