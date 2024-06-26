{ pkgs, ... }:

let
  blocky-tentacool = pkgs.writeShellScriptBin "blocky" ''
    exec ${pkgs.blocky}/bin/blocky --apiHost tentacool "$@"
  '';
in
{
  programs.bat = {
    enable = true;
    catppuccin.enable = true;
  };

  home.file."Library/Preferences/glow/glow.yml".text = ''
    local: true
    pager: true
  '';

  home.packages = with pkgs; [
    ast-grep
    blocky-tentacool
    dogdns
    fd
    gdu
    glow
    ripgrep
    sd
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
