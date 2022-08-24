{ pkgs }:

pkgs.fishPlugins.buildFishPlugin rec {
  pname = "fish-minimal-theme";
  version = "9cabe0f044bb80bcbfec7d6804971836003df681";

  src = builtins.fetchTarball {
    url = "https://github.com/samhh/fish-minimal-theme/archive/${version}.tar.gz";
    sha256 = "1ilmnjxsaqzkjlqdn2m348bfjg23k6dkcak5p7qb84yz13pf3dfv";
  };
}
