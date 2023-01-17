{ config, lib, pkgs, ... }:

let
  editorBin = "${config.home-manager.users.${config.username}.programs.neovim.finalPackage}/bin/nvim";

  # Workaround for incompatibililty between nixpkgs fish plugins and
  # home-manager:
  #   https://github.com/nix-community/home-manager/issues/2451
  #
  # Can't use `pkgs.wrapFish` directly as it drops everything except the
  # binary, and Home Manager relies upon other stuff in the package such
  # as a script for completions generation.
  customFish =
    pkgs.fish.overrideAttrs (attrs: {
      fixupPhase =
        let
          overrides = {
            pluginPkgs = with pkgs.fishPlugins; [ fish-minimal-theme ];
          };
        in
        ''
          cp ${pkgs.wrapFish overrides}/bin/fish $out/bin/fish
        '';
    });

in
{
  users.users.${config.username}.shell = customFish;

  home-manager.users.${config.username} = {
    programs.fish = {
      enable = true;
      package = customFish;
      shellInit = ''
        set fish_greeting
        fish_vi_key_bindings

        source ${pkgs.vimPlugins.kanagawa-nvim}/extras/kanagawa.fish

        set -x VISUAL ${editorBin}
        set -x EDITOR ${editorBin}
        set -x MANPAGER ${editorBin} +Man!
        set -x DIFFPROG ${editorBin} -d
        set -x DIRENV_LOG_FORMAT ""
      '';
      shellAbbrs = {
        "diff" = "nvim -d";
        "grep" = "rg";
        "vi" = "nvim";

        "sh" = "nix shell";
        "nixh" = "nix-prefetch-url";
        "nixhu" = "nix-prefetch-url --unpack";
      };
      functions = {
        mkcd = "mkdir -p $argv; cd $argv;";
        mktouch = "mkdir -p (dirname $argv); touch $argv;";
      };
    };

    # The standard `command-not-found` functionality is incompatible with a
    # flakes-only setup. This is a userland alternative. See:
    #
    #   https://discourse.nixos.org/t/command-not-found-unable-to-open-database/3807/6
    #   https://github.com/nixos/nixpkgs/issues/39789#issuecomment-1037931569
    #   https://github.com/nixos/nixpkgs/pull/187894#issuecomment-1234292290
    #
    # Remember to manually run `nix-index` at least once to generate the index.
    programs.nix-index.enable = true;

    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    programs.zoxide.enable = true;

    programs.git.ignores = [
      "flake.nix"
      "flake.lock"
      "shell.nix"
      "result"
      ".envrc"
      ".direnv/"
    ];
  };
}
