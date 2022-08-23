{ fetchurl, stdenv }:

stdenv.mkDerivation rec {
  pname = "proton-ge-custom";
  version = "GE-Proton7-30";

  src = fetchurl {
    url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/${version}/${version}.tar.gz";
    sha512 = "450f6379d452d6e3bce94fecbcc9f207051bf369bd7c63b325f97c46a10901a5ab7391cb09e841cefa2e98bd30383b8409ca81a6629929f08149bb45f22f8bf2";
  };

  installPhase = ''
    mkdir -p $out
    mv * $out/
  '';
}
