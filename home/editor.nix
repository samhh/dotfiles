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

    languages.language = [
      {
        name = "typescript";
        config.plugins = [
          {
            name = "typescript-tshm-plugin";
            location = "/Users/sam/.local/share/npmlibs/";
          }
        ];
      }
    ];
  };

  # TODO: Awaiting multi-LSP support in Helix.
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

  home.packages = with pkgs; [
    # Language servers
    ## Don't install HLS as it's version-dependent. Instead install in the
    ## project's Nix dev shell.
    nodePackages.bash-language-server
    dhall-lsp-server
    nodePackages.purescript-language-server
    rnix-lsp
    rust-analyzer
    # Needed for rust-analyzer.
    rustc
    nodePackages.typescript-language-server
    nodePackages.vscode-langservers-extracted

    # Tools w/ language server interop
    hlint
    stylish-haskell
  ];
}
