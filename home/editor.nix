{ pkgs-unstable, ... }:

{
  programs.helix = {
    enable = true;

    settings = {
      theme = "kanagawa";

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

    languages = {
      language-server = {
        typescript-language-server = {
          # These are the defaults:
          #   https://github.com/helix-editor/helix/blob/23.10/languages.toml#L132
          command = "typescript-language-server";
          args = [ "--stdio" ];
          config.hostInfo = "helix";

          config.plugins = [
            {
              name = "typescript-tshm-plugin";
              location = "/Users/sam/.local/share/npmlibs/";
            }
          ];
        };

        biome = {
          command = "biome";
          args = [ "lsp-proxy" ];
        };

        eslint = {
          command = "vscode-eslint-language-server";
          args = [ "--stdio" ];
          config = {
            experimental.useFlatConfig = true;

            # These seem to be necessary for the language server to start at
            # all, see:
            #   https://github.com/helix-editor/helix/issues/3520#issuecomment-1439987347
            validate = "on";
            rulesCustomizations = [ ];
            run = "onType";
            problems.shortenToSingleLine = false;
            nodePath = "";
          };
        };

        snippets-ls = {
          command = "snippets-ls";
          args = [ "-config" ./snippets/typescript.json ];
        };
      };

      language = [
        {
          name = "typescript";
          language-servers = [
            {
              name = "typescript-language-server";
              except-features = [ "format" ];
            }

            "biome"

            # There's an annoying error in Unsplash Web at the top of files on
            # load that doesn't surface via the CLI.
            # "eslint"

            "snippets-ls"
          ];
          # This shouldn't be necessary but it doesn't work through LSP, see:
          #   https://github.com/biomejs/biome/pull/1031#issuecomment-1906278211
          formatter = {
            command = "biome";
            args = [ "format" "--stdin-file-path" "foo.ts" ];
          };
        }
      ];
    };

    # Don't install HLS as it's version-dependent.
    extraPackages = with pkgs-unstable; [
      nodePackages.bash-language-server
      biome
      dhall-lsp-server
      # For HLS.
      hlint
      nil
      nodePackages.purescript-language-server
      rust-analyzer
      # Needed for rust-analyzer.
      rustc
      snippets-ls
      terraform-ls
      nodePackages.typescript-language-server
      nodePackages.vscode-langservers-extracted
    ];
  };

  programs.git.ignores = [
    ".helix/"
  ];

  programs.tshm = {
    enable = true;
    installEditorPlugin = true;
  };
}
