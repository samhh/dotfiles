{ pkgs, ... }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    deadnix
  ];
}
