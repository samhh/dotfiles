{ pkgs, ... }:

{
  exrc-vim = pkgs.callPackage ./exrc-vim.nix { };
  eyeliner-nvim = pkgs.callPackage ./eyeliner-nvim.nix { };
  nvim-surround = pkgs.callPackage ./nvim-surround.nix { };
  vim-just = pkgs.callPackage ./vim-just.nix { };
}
