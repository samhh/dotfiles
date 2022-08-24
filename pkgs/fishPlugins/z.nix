{ pkgs }:

pkgs.fishPlugins.buildFishPlugin rec {
  pname = "z";
  version = "45a9ff6d0932b0e9835cbeb60b9794ba706eef10";

  src = builtins.fetchTarball {
    url = "https://github.com/jethrokuan/z/archive/${version}.tar.gz";
    sha256 = "1kjyl4gx26q8175wcizvsm0jwhppd00rixdcr1p7gifw6s308sd5";
  };
}
