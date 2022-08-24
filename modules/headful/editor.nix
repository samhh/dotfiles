{ pkgs, tshmPlugin, uname, ... }:

let
  tshm = pkgs.callPackage ../../pkg/tshm.nix { };

  exrc-vim = pkgs.vimUtils.buildVimPlugin {
    name = "exrc.vim";
    src = builtins.fetchTarball {
      url = "https://github.com/ii14/exrc.vim/archive/ae734ae2c087b370d869e41a2706a128d8f3fc37.tar.gz";
      sha256 = "0jadpcg3hsfzbglh21zlfhj2d9ymyh73p3kd4wd9imlhdhsx99d7";
    };
    # Tests run via `make` will fail as they're expecting `vim`.
    dontBuild = true;
  };

  vim-just = pkgs.vimUtils.buildVimPlugin {
    name = "vim-just";
    src = builtins.fetchTarball {
      url = "https://github.com/noahtheduke/vim-just/archive/312615d5b4c4aa2595d697faca5af345ba8fe102.tar.gz";
      sha256 = "05c2qdnrjvxshy48m0s6msvqq47n536p8c4dvf0j28hm39hqb8gj";
    };
  };

  nvim-surround = pkgs.vimUtils.buildVimPlugin {
    name = "nvim-surround";
    src = builtins.fetchTarball {
      url = "https://github.com/kylechui/nvim-surround/archive/a533ff9f9d7ba85d48567e2c055e16cb0923a27d.tar.gz";
      sha256 = "1v6k45zdlspl8xqs39pjdmvb6vagpcy7r694r0z12lhwq9x3kz4i";
    };
  };

  eyeliner-nvim = pkgs.vimUtils.buildVimPlugin {
    name = "eyeliner.nvim";
    src = builtins.fetchTarball {
      url = "https://github.com/jinh0/eyeliner.nvim/archive/7b8ce8c1e0b466328e02e7649c759dc96ed457aa.tar.gz";
      sha256 = "08l7gyf5g4cw47hxnjph6g8r3ldvzwv4z9czzyf14rn0glri0hwb";
    };
  };

in
{
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
        eyeliner-nvim

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

    home.packages =
      let
        distroSpecific =
          if pkgs.stdenv.isDarwin
          then [ ]
          else with pkgs; [ haskell-language-server tshm ];
      in
      with pkgs; [
        # For :TSUpdate
        gcc

        # Language servers
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
