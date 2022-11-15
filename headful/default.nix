{ nix-colors, ... }:

{
  imports = [
    nix-colors.homeManagerModule
    ./editor.nix
    ./terminal.nix
    ./vcs.nix
  ];
}
