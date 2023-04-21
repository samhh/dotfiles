{ fetchurl, lib, pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "tshm";
  version = "0.4.2";

  src = fetchurl {
    url = "https://github.com/samhh/tshm/releases/download/${version}/tshm-${version}-linux-x86_64";
    sha256 = "0bcc029kricsnz3ixqknrnl99hgdi07nk5r8gzj0kz6d5xal2c3k";
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
