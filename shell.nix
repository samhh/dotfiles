{ pkgs, ... }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    agenix
    deadnix
    nushell
  ];
}
