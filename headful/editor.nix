{ config, pkgs, tshmPlugin, ... }:

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

        # Formatting
        editorconfig-nvim

        # Compilation
        vim-dispatch

        # LSP
        nvim-lspconfig

        # Syntax
        nvim-treesitter.withAllGrammars
        nvim-treesitter-refactor
        vim-matchup
        dhall-vim
        purescript-vim
        vim-just

        # Theming
        lightline-vim
        kanagawa-nvim
      ];
    };

    xdg.configFile = {
      nvim = {
        source = ./cfg/nvim;
        recursive = true;
      };
      "efm-langserver/config.yaml".source = ./cfg/efm.yaml;
    };

    xdg.dataFile."npmlibs/node_modules/typescript-tshm-plugin".source = tshmPlugin;

    programs.git.ignores = [
      ".exrc"
    ];

    home.packages =
      let
        distroSpecific =
          if pkgs.stdenv.isDarwin
          then [ ]
          else with pkgs; [ tshm ];
      in
      with pkgs; [
        # For :TSUpdate
        gcc

        # Language servers
        ## Don't install HLS as it's version-dependent. Instead install in the
        ## project's Nix dev shell.
        nodePackages.bash-language-server
        dhall-lsp-server
        efm-langserver
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
      ] ++ distroSpecific;
  };
}
