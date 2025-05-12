{ pkgs, ... }:

{
  programs.bat.enable = true;

  home.packages = with pkgs; [
    ast-grep
    fd
    gdu
    ripgrep
    sd
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
