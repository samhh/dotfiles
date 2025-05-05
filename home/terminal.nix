# Ghostty's installation is managed outside of Nix.

{ pkgs, lib, ... }:

{
  programs.ghostty = {
    enable = true;
    package = pkgs.noop;
    # https://github.com/nix-community/home-manager/pull/6235#issuecomment-2567896192
    installBatSyntax = false;
    settings = {
      background-blur-radius = 20;
      background-opacity = 0.92;
      cursor-style-blink = false;
      font-size = 12;
      keybind = "global:super+`=toggle_quick_terminal";
      quick-terminal-animation-duration = 0;
      quick-terminal-position = "center";
      shell-integration-features = "no-cursor";
      window-padding-x = 8;
      window-padding-y = 8;

      # catppuccin currently only supports one universal theme:
      #   https://github.com/catppuccin/nix/issues/420
      theme = lib.mkForce "light:catppuccin-latte,dark:catppuccin-mocha";
    };
  };
}
