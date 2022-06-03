{ pkgs, tshmPlugin, uname, ... }:

let
  tshm = pkgs.callPackage ../../pkg/tshm.nix {};

  exrc-vim = pkgs.vimUtils.buildVimPlugin {
    name = "exrc.vim";
    src = builtins.fetchTarball {
      url = "https://github.com/ii14/exrc.vim/archive/ae734ae2c087b370d869e41a2706a128d8f3fc37.tar.gz";
      sha256 = "0jadpcg3hsfzbglh21zlfhj2d9ymyh73p3kd4wd9imlhdhsx99d7";
    };
  };

  # Included in more recent nixpkgs.
  mkdir-nvim = pkgs.vimUtils.buildVimPlugin {
    name = "mkdir.nvim";
    src = builtins.fetchTarball {
      url = "https://github.com/jghauser/mkdir.nvim/archive/01261650382bef195dab8ac39344234b57914f09.tar.gz";
      sha256 = "1irpi2aqi2pr0ydxsw2d4m2lkhzkqcs6gvz15snvnsckvk03j3v7";
    };
  };

  vim-just = pkgs.vimUtils.buildVimPlugin {
    name = "vim-just";
    src = builtins.fetchTarball {
      url = "https://github.com/noahtheduke/vim-just/archive/312615d5b4c4aa2595d697faca5af345ba8fe102.tar.gz";
      sha256 = "05c2qdnrjvxshy48m0s6msvqq47n536p8c4dvf0j28hm39hqb8gj";
    };
  };
in {
  home-manager.users.${uname} = {
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

        # Layout
        vim-bbye
        gitsigns-nvim
        vim-trailing-whitespace

        # Motions & Objects
        comment-nvim
        vim-surround
        vim-grepper
        vim-closer
        hop-nvim
        gitlinker-nvim
        targets-vim

        # Formatting
        editorconfig-vim

        # Compilation
        vim-dispatch

        # LSP
        nvim-lspconfig

        # Syntax
        nvim-treesitter
        nvim-treesitter-refactor
        vim-matchup
        dhall-vim
        haskell-vim
        purescript-vim
        vim-fish
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

    xdg.dataFile."typescript-tshm-plugin".source = tshmPlugin;

    programs.git.ignores = [
      ".exrc"
    ];

    home.packages = with pkgs; [
      # For :TSUpdate
      gcc

      # Language servers
      nodePackages.bash-language-server
      dhall-lsp-server
      efm-langserver
      haskell-language-server
      rnix-lsp
      rust-analyzer
      # Needed for rust-analyzer.
      rustc
      nodePackages.typescript-language-server
      nodePackages.vscode-langservers-extracted

      # Tools w/ language server interop
      hlint
      stylish-haskell
      tshm
    ];
  };
}
