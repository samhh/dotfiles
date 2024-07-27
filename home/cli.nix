{ pkgs, ... }:

let
  blocky-homelab = pkgs.writeShellScriptBin "blocky" ''
    exec ${pkgs.blocky}/bin/blocky --apiHost homelab "$@"
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
    blocky-homelab
    fd
    gdu
    glow
    ripgrep
    sd
    tldr
    tre-command
    nodePackages.yalc

    # Unsplash
    okta-aws-cli
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
