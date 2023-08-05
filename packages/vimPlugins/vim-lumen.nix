{ pkgs }:

pkgs.vimUtils.buildVimPlugin rec {
  pname = "vim-lumen";
  version = "b183859510bebfc9062caf300e24c707a5fe522f";
  src = builtins.fetchTarball {
    url = "https://github.com/vimpostor/vim-lumen/archive/${version}.tar.gz";
    sha256 = "0ckjznnx9ia4pj6lvqdz380n6702s6n8kamxr7klb43wwgsraphs";
  };
}
