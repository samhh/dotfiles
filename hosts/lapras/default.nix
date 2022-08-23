{ ... }:

{
  imports = [
    ../../modules/editor.nix
    ../../modules/terminal.nix
    ../../modules/vcs.nix

    ./auth.nix
    ./core.nix
    ./misc.nix
  ];
}
