{ fetchurl, lib, stdenv }:

stdenv.mkDerivation rec {
  pname = "proton-ge-custom";
  version = "GE-Proton7-55";

  src = fetchurl {
    url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/${version}/${version}.tar.gz";
    sha512 = "8fa9ad9d0e1957ced72cf48a0e5234203b4abec28bd039df8f57aea71d7fe8da5e1cbef0d208d324ebc77559b0e278abf54aa7f6c15bfcb4fb1a136de0652903";
  };

  installPhase = ''
    mkdir -p $out
    mv * $out/
  '';

  meta.platforms = lib.platforms.linux;
}
