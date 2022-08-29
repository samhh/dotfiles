{ config, lib, pkgs, uname, webBrowserBin, ... }:

let
  editorBin = "${config.home-manager.users.${uname}.programs.neovim.finalPackage}/bin/nvim";

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
            pluginPkgs = with pkgs.fishPlugins; [
              fish-minimal-theme
              z
            ];
          };
        in
        ''
          cp ${pkgs.wrapFish overrides}/bin/fish $out/bin/fish
        '';
    });

in
{
  users.users.${uname}.shell = customFish;

  home-manager.users.${uname} = {
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
        "nixu" = "nix-prefetch-url --unpack";
      };
      functions = {
        mkcd = "mkdir -p $argv; cd $argv;";
        mktouch = "mkdir -p (dirname $argv); touch $argv;";
      };
    };

    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    programs.git.ignores = [
      "shell.nix"
      "result"
      ".envrc"
      ".direnv/"
    ];
  };
}
