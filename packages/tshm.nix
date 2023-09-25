{ fetchurl, lib, stdenv }:

stdenv.mkDerivation rec {
  pname = "tshm";
  version = "0.4.2";

  src = fetchurl {
    url = "https://github.com/samhh/tshm/releases/download/${version}/tshm-${version}-macos-x86_64";
    sha256 = "0mrw84pz4hjx6rzzmnj1kzvz8wgh0hxpviq3f9gj7wg7c1hvickm";
  };

  dontUnpack = true;
  dontStrip = true;

  installPhase = ''
    mkdir -p $out/bin/
    cp $src $out/bin/tshm
    chmod +x $out/bin/tshm
  '';

  meta = {
    homepage = "https://github.com/samhh/tshm";
    description = "A parser and formatter for TypeScript declarations that outputs HM-style type signatures.";
    license = lib.licenses.mit;
  };
}
