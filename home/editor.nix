{ lib, pkgs, ... }:

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

        # This is currently a bit opinionated in terms of the binary being
        # sourced by Yarn.
        biome = {
          command = "yarn";
          args = [ "run" "biome" "lsp-proxy" ];
        };

        snippets-ls = {
          command = "snippets-ls";
          args = [ "-config" ./snippets/typescript.json ];
        };

        efm = {
          command = "efm-langserver";
          config.documentFormatting = true;
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

            {
              name = "biome";
              except-features = [ "format" ];
            }

            {
              name = "efm";
              only-features = [ "format" ];
            }

            "snippets-ls"
          ];
        }
      ];
    };

    # Don't install HLS as it's version-dependent.
    extraPackages = with pkgs; [
      nodePackages.bash-language-server
      dhall-lsp-server
      # For HLS.
      hlint
      nil
      nodePackages.purescript-language-server
      rust-analyzer
      # Needed for rust-analyzer.
      rustc
      snippets-ls
      nodePackages.typescript-language-server
      nodePackages.vscode-langservers-extracted
    ];
  };

  programs.efm-langserver = {
    enable = true;
    languages =
      let
        js = [ "javascript" "typescript" "typescriptreact" ];
        prettier = {
          root-markers = [ "node_modules/prettier/" ];
          format-command = "./node_modules/.bin/prettier --stdin-filepath \${INPUT}";
          format-stdin = true;
        };
        f = x: { name = x; value = [ prettier ]; };
      in
      lib.listToAttrs (map f js);
  };

  programs.tshm = {
    enable = true;
    installEditorPlugin = true;
  };
}
