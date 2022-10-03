{ pkgs }:

pkgs.vimUtils.buildVimPlugin {
  pname = "eyeliner.nvim";
  version = "f13c8a19bd23db6ead7424b29f064279f3546738";
  src = builtins.fetchTarball {
    url = "https://github.com/jinh0/eyeliner.nvim/archive/f13c8a19bd23db6ead7424b29f064279f3546738.tar.gz";
    sha256 = "0cb5vsh6ivlwndbaq2ym0rymaziwjnjw2aw22jwzpkyd2162rwzy";
  };
}
