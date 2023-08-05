{ fetchurl, lib, pkgs }:

pkgs.buildGoModule rec {
  pname = "corrupter";
  # v1.0 didn't include a `go.mod`.
  version = "76bcd3203c86c354bd2fdbc704018475e7303ac0";

  src = fetchurl {
    url = "https://github.com/r00tman/corrupter/archive/${version}.tar.gz";
    sha256 = "10vsm3497x322szqdqp3m8swblp2b8gns6wwk8m3w668xv6fcvl6";
  };

  vendorSha256 = null;

  meta.platforms = lib.platforms.linux;
}
