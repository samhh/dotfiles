{ pkgs, ... }:

{
  exrc-vim = pkgs.callPackage ./exrc-vim.nix { };
  vim-just = pkgs.callPackage ./vim-just.nix { };
  vim-lumen = pkgs.callPackage ./vim-lumen.nix { };
}
