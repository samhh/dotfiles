{ pkgs }:

pkgs.vimUtils.buildVimPlugin rec {
  pname = "vim-just";
  version = "312615d5b4c4aa2595d697faca5af345ba8fe102";
  src = builtins.fetchTarball {
    url = "https://github.com/noahtheduke/vim-just/archive/${version}.tar.gz";
    sha256 = "05c2qdnrjvxshy48m0s6msvqq47n536p8c4dvf0j28hm39hqb8gj";
  };
}
