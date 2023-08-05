{ pkgs }:

pkgs.fishPlugins.buildFishPlugin rec {
  pname = "fish-minimal-theme";
  version = "f1d159b9d6b6f212f4de143afc743eef45a29533";

  src = builtins.fetchTarball {
    url = "https://github.com/samhh/fish-minimal-theme/archive/${version}.tar.gz";
    sha256 = "07nfnajmvhr3908gvjgx83rfnxm5pnvri6ahybyqhs7cqpnl8h4q";
  };
}
