{ pkgs, ... }:

{
  exrc-vim = pkgs.callPackage ./exrc-vim.nix { };
  nvim-surround = pkgs.callPackage ./nvim-surround.nix { };
  vim-just = pkgs.callPackage ./vim-just.nix { };
  vim-lumen = pkgs.callPackage ./vim-lumen.nix { };
}
