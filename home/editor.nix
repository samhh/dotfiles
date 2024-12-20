{ pkgs, pkgs-unstable, ... }:

{
  programs.helix = {
    enable = true;
    settings = {
      editor = {
        soft-wrap.enable = true;
        rulers = [ 80 ];
      };

      keys =
        let
          movement = {
            "{" = "goto_prev_paragraph";
            "}" = "goto_next_paragraph";
          };
        in
        {
          normal = movement // {
            # https://github.com/helix-editor/helix/issues/3001
            c = "change_selection_noyank";
            "A-c" = "change_selection";
            d = "delete_selection_noyank";
            "A-d" = "delete_selection";

            space = {
              l = "file_picker_in_current_buffer_directory";
              q = ":buffer-close";
              Q = ":buffer-close-all";
              z = ":format";
            };
          };

          select = movement // {
            space.q = ":reflow";
          };
        };
    };
  };

  programs.zed-editor = {
    enable = true;
    package = pkgs-unstable.zed-editor;
    extensions = [
      "catppuccin"

      "biome"
      "dockerfile"
      "haskell"
      "html"
      "nix"
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
      theme = {
        light = "Catppuccin Latte - No Italics";
        dark = "Catppuccin Mocha - No Italics";
      };
      git.inline_blame.enabled = false;
      wrap_guides = [ 80 ];
    };
  };

  xdg.configFile."zed/snippets".source = ./snippets;

  programs.git.ignores = [
    ".helix/"
    ".zed/"
  ];

  home.packages = with pkgs; [
    # For Biome language server in Zed.
    nodejs
  ];
}
