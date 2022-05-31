{ fetchurl, stdenv }:

stdenv.mkDerivation rec {
  pname = "proton-ge-custom";
  version = "GE-Proton7-10";

  src = fetchurl {
    url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/${version}/${version}.tar.gz";
    sha256 = "0i7m4cyj47ggk0gc6argsvpxfyagx4q4258dlrf968dvi9wxd6xf";
  };

  installPhase = ''
    mkdir -p $out
    mv * $out/
  '';
}
