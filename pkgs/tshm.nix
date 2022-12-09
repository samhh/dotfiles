{ fetchurl, lib, pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "tshm";
  version = "0.4.0";

  src = fetchurl {
    url = "https://github.com/samhh/tshm/releases/download/${version}/tshm-linux-amd64";
    sha256 = "03nnhmplmn2mqibqclyr28dhh1v0f4dn18k4rnbvc9xy2l4k32xf";
  };

  dontUnpack = true;
  dontStrip = true;

  installPhase = ''
    mkdir -p $out/bin/
    cp $src $out/bin/tshm
    chmod +x $out/bin/tshm
  '';

  preFixup =
    let
      patchelf = "${pkgs.patchelf}/bin/patchelf";
      linker = stdenv.cc.bintools.dynamicLinker;
      libPath = with pkgs; lib.makeLibraryPath [ gmp ];
    in
    ''
      ${patchelf} --set-interpreter ${linker} --set-rpath ${libPath} $out/bin/tshm
    '';

  meta = {
    homepage = "https://github.com/samhh/tshm";
    description = "A parser and formatter for TypeScript declarations that outputs HM-style type signatures.";
    license = lib.licenses.mit;
  };
}
