{ pkgs }:

pkgs.vimUtils.buildVimPlugin {
  name = "eyeliner.nvim";
  src = builtins.fetchTarball {
    url = "https://github.com/jinh0/eyeliner.nvim/archive/7b8ce8c1e0b466328e02e7649c759dc96ed457aa.tar.gz";
    sha256 = "08l7gyf5g4cw47hxnjph6g8r3ldvzwv4z9czzyf14rn0glri0hwb";
  };
}
