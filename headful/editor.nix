{ config, lib, pkgs, tshmPlugin, ... }:

{
  home-manager.users.${config.username} = {
    programs.neovim = {
      enable = true;
      package = pkgs.neovim-unwrapped;

      withNodeJs = false;
      withPython3 = false;
      withRuby = false;

      plugins = with pkgs.vimPlugins; [
        # General
        exrc-vim
        mkdir-nvim
        plenary-nvim
        telescope-nvim
        telescope-fzf-native-nvim
        telescope-ui-select-nvim

        # Layout
        vim-bbye
        gitsigns-nvim
        vim-trailing-whitespace

        # Motions & Objects
        comment-nvim
        nvim-surround
        vim-grepper
        vim-closer
        hop-nvim
        gitlinker-nvim
        targets-vim

        # Snippets
        nvim-snippy

        # LSP
        nvim-lspconfig

        # Syntax
        nvim-treesitter.withAllGrammars
        nvim-treesitter-refactor
        vim-matchup
        purescript-vim
        vim-just

        # Theming
        lightline-vim
        kanagawa-nvim
      ];
    };

    xdg.configFile.nvim = {
      source = ./cfg/nvim;
      recursive = true;
    };

    programs.git.ignores = [
      ".exrc"
    ];

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
      enable = pkgs.stdenv.isLinux;
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
  };
}
