{ pkgs, pkgs-unstable, ... }:

{
  programs.bat = {
    enable = true;
    catppuccin.enable = true;
  };

  home.packages = with pkgs; [
    pkgs-unstable.ast-grep
    fd
    gdu
    ripgrep
    sd
    tldr
    tre-command

    # Unsplash
    okta-aws-cli
  ];

  programs.git.ignores = [
    # PureScript
    ".psc-ide-port"
    ".psci_modules/"

    # npm
    ".npmrc"
  ];
}
