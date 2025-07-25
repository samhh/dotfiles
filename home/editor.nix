# Zed's installation is managed outside of Nix.

{ lib, pkgs, ... }:

{
  programs.helix = {
    enable = true;
    settings.editor.rulers = [ 80 ];
  };

  programs.zed-editor = {
    enable = true;
    package = pkgs.noop;
    extensions = [
      "biome"
      "dockerfile"
      "haskell"
      "html"
      "just"
      "nix"
      "nu"
      "purescript"
      "terraform"
      "toml"
    ];
    userSettings = {
      agent = {
        version = "2";
        default_model = {
          provider = "zed.dev";
          model = "claude-sonnet-4";
        };
      };
      edit_predictions.mode = "subtle";
      diagnostics.inline.enabled = true;
      ui_font_size = 14;
      buffer_font_size = 12;
      buffer_font_fallbacks = [ "FiraCode Nerd Font Mono" ];
      git.inline_blame.enabled = false;
      wrap_guides = [ 80 ];
      terminal.env.VISUAL = "zed --wait";
      helix_mode = true;

      languages.Nix.language_servers = [
        "nil"
        "!nixd"
      ];
      lsp = {
        nil = {
          binary.path = lib.getExe pkgs.nil;
          settings = {
            formatting.command = [ (lib.getExe pkgs.nixfmt-rfc-style) ];
            nix.flake.autoEvalInputs = true;
          };
        };
      };

      # catppuccin currently only supports one universal theme:
      #   https://github.com/catppuccin/nix/issues/420
      icon_theme = lib.mkForce {
        light = "Catppuccin Latte";
        dark = "Catppuccin Mocha";
      };
      theme.light = lib.mkForce "Catppuccin Latte - No Italics";
    };
  };

  xdg.configFile."zed/snippets".source = ./snippets;

  programs.git.ignores = [
    ".helix/"
    ".zed/"
  ];
}
