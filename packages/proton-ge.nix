{ fetchurl, lib, stdenv }:

stdenv.mkDerivation rec {
  pname = "proton-ge-custom";
  version = "GE-Proton8-3";

  src = fetchurl {
    url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/${version}/${version}.tar.gz";
    sha512 = "2a5f04d42e685c19c5fe590d26734f1179f1d12a25a58e5023df7eac2ad1d333e2abe3e3f18b35f2a6610fd5f1816ed2f426fbf63e6ddef4f84a24f87289072b";
  };

  installPhase = ''
    mkdir -p $out
    mv * $out/
  '';

  meta.platforms = lib.platforms.linux;
}
