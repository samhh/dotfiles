{ pkgs }:

pkgs.vimUtils.buildVimPlugin {
  pname = "nvim-surround";
  version = "a533ff9f9d7ba85d48567e2c055e16cb0923a27d";
  src = builtins.fetchTarball {
    url = "https://github.com/kylechui/nvim-surround/archive/a533ff9f9d7ba85d48567e2c055e16cb0923a27d.tar.gz";
    sha256 = "1v6k45zdlspl8xqs39pjdmvb6vagpcy7r694r0z12lhwq9x3kz4i";
  };
}
