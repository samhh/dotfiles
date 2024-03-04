{ pkgs, ... }:

let
  blocky-tentacool = pkgs.writeShellScriptBin "blocky" ''
    exec ${pkgs.blocky}/bin/blocky --apiHost tentacool "$@"
  '';
in
{
  home.packages = with pkgs; [
    ast-grep
    bat
    blocky-tentacool
    dogdns
    fd
    gdu
    ripgrep
    sd
    shellcheck
    tldr
    tre-command
    nodePackages.yalc

    # Unsplash
    saml2aws
  ];

  programs.git.ignores = [
    # Yalc
    ".yalc/"
    "yalc.lock"

    # PureScript
    ".psc-ide-port"
    ".psci_modules/"

    # npm
    ".npmrc"

    # Obsidian
    ".obsidian/"
    ".trash/"
  ];
}
