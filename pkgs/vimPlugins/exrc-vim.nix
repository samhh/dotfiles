{ pkgs }:

pkgs.vimUtils.buildVimPlugin {
  name = "exrc.vim";
  src = builtins.fetchTarball {
    url = "https://github.com/ii14/exrc.vim/archive/ae734ae2c087b370d869e41a2706a128d8f3fc37.tar.gz";
    sha256 = "0jadpcg3hsfzbglh21zlfhj2d9ymyh73p3kd4wd9imlhdhsx99d7";
  };
  # Tests run via `make` will fail as they're expecting `vim`.
  dontBuild = true;
}