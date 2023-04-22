{ nix-colors, ... }:

{
  imports = [
    nix-colors.homeManagerModules.default
    ./editor.nix
    ./terminal.nix
    ./vcs.nix
  ];
}
