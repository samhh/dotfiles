let
  pkgs = import <nixpkgs> {};

in
  pkgs.mkShell rec {
    buildInputs = with pkgs; [
      git
      stow
    ];
  }
