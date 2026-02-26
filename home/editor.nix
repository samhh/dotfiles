{ lib, pkgs, ... }:

{
  programs.helix = {
    enable = true;
    settings.editor.rulers = [ 80 ];
  };

  # Configuring Zed purely in Nix is poor UX as, among other reasons, lots of
  # little configs (e.g. agents) are best managed through the UI. This config is
  # therefore disabled and instead acts as a backup of important settings.
  # Ideally there'd be settings sync:
  #   https://github.com/zed-industries/zed/discussions/6569
  programs.zed-editor = {
    enable = false;
    package = null;
    extensions = [
      "comment"
      "mcp-server-context7"
    ];
    userSettings = {
      edit_predictions.mode = "subtle";
      diagnostics.inline.enabled = true;
      ui_font_size = 14;
      buffer_font_size = 12;
      buffer_font_fallbacks = [ "FiraCode Nerd Font Mono" ];
      confirm_quit = true;
      git.inline_blame.enabled = false;
      wrap_guides = [ 100 ];
      sticky_scroll.enabled = true;
      terminal.env.VISUAL = "zed --wait";
      # catppuccin currently only supports one universal theme:
      #   https://github.com/catppuccin/nix/issues/420
      icon_theme = lib.mkForce {
        light = "Catppuccin Latte";
        dark = "Catppuccin Mocha";
      };
      theme.light = lib.mkForce "Catppuccin Latte - No Italics";
    };
  };

  xdg.configFile."zed/snippets/typescript.json".source = ./snippets/typescript.json;
  xdg.configFile."zed/snippets/tsx.json".source = ./snippets/typescript.json;

  programs.git.ignores = [
    ".helix/"
    ".zed/"
  ];

  home.packages =
    with pkgs;
    let
      codex = writeShellScriptBin "codex" ''
        exec /Applications/Codex.app/Contents/Resources/codex "$@"
      '';
      skills = writeShellApplication {
        name = "skills";

        runtimeInputs = [
          nodejs
          pnpm
        ];

        text = ''
          exec pnpx skills "$@"
        '';
      };
    in
    [
      codex
      skills
    ];
}
