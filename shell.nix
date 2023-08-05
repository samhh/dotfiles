{ pkgs, ... }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    agenix
    git-crypt
    nixpkgs-fmt
  ];
}
