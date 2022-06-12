{ fetchurl, lib, pkgs, stdenv }:

let
  bangin = pkgs.callPackage ./bangin.nix {};
in
  stdenv.mkDerivation rec {
    pname = "bangin-server-node";
    version = "0.1.1";

    src = fetchurl {
      url = "https://git.sr.ht/~samhh/bangin-server-node/archive/${version}.tar.gz";
      sha256 = "14pnqljbgxk7gncd7k59mk889kc5z6wl0asbxpp6si6rwg7malnx";
    };

    nativeBuildInputs = with pkgs; [
      gnused
      makeWrapper
    ];

    configurePhase = ''
      sed -i '1i #!/usr/bin/env node' index.js
      chmod +x index.js
    '';

    installPhase = ''
      mkdir -p $out/bin/
      mv index.js $out/bin/bangin-server-node
    '';

    fixupPhase = with pkgs; ''
      wrapProgram $out/bin/bangin-server-node \
        --set PATH ${lib.makeBinPath [ nodejs bangin ]}
    '';

    meta = {
      homepage = "https://git.sr.ht/~samhh/bangin-server-node";
      description = "A basic web server around bangin written in Node.js.";
      license = lib.licenses.mit;
    };
  }
