# Zed's installation is managed outside of Nix.

{ lib, pkgs, ... }:

{
  programs.helix = {
    enable = true;
    settings.editor.rulers = [ 80 ];
  };

  programs.zed-editor = {
    enable = true;
    package = pkgs.emptyDirectory;
    extensions = [
      "biome"
      "dockerfile"
      "haskell"
      "html"
      "nix"
      "nu"
      "purescript"
      "terraform"
      "toml"
    ];
    userSettings = {
      assistant = {
        default_model = {
          provider = "zed.dev";
          model = "claude-3-5-sonnet-latest";
        };
        version = "2";
      };
      ui_font_size = 14;
      buffer_font_size = 12;
      buffer_font_fallbacks = [ "FiraCode Nerd Font Mono" ];
      git.inline_blame.enabled = false;
      wrap_guides = [ 80 ];
      terminal.env.VISUAL = "zed --wait";

      languages.Nix.language_servers = [
        "nil"
        "!nixd"
      ];
      lsp = {
        nil = {
          binary.path = "${pkgs.nil}/bin/nil";
          settings = {
            formatting.command = [ "${pkgs.nixfmt-rfc-style}/bin/nixfmt" ];
            nix.flake.autoEvalInputs = true;
          };
        };
      };

      # catppuccin currently doesn't support icons in Zed:
      #   https://github.com/catppuccin/nix/pull/467
      # And Zed doesn't currently support light/dark icon themes:
      #   https://github.com/zed-industries/zed/discussions/24370
      icon_theme = "Catppuccin Latte";

      # catppuccin currently only supports one universal theme:
      #   https://github.com/catppuccin/nix/issues/420
      theme.light = lib.mkForce "Catppuccin Latte - No Italics";
    };
  };

  xdg.configFile."zed/snippets".source = ./snippets;

  programs.git.ignores = [
    ".helix/"
    ".zed/"
  ];
}
