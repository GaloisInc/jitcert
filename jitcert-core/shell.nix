# <<<<<<< HEAD
# { ghcver ? "ghc864" }:
# (import ./nixbuild/JitCert-project.nix { inherit ghcver; }).JitCert.env
# =======
{ nixpkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.03.tar.gz) {} }:

let

  inherit (nixpkgs) pkgs;
  f = { mkDerivation, aeson, base, bytestring, casing, containers
      , either, fgl, filepath, graphviz, HUnit, mtl, parameterized-utils
      , prettyprinter, process, SHA, stdenv, text, time, utf8-string, word-wrap
      , yaml
      }:
      mkDerivation {
        pname = "JitCert";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base bytestring casing containers fgl filepath graphviz mtl
          parameterized-utils prettyprinter process SHA text time utf8-string
          word-wrap yaml
        ];
        executableHaskellDepends = [
          aeson base bytestring casing containers either fgl filepath
          graphviz mtl parameterized-utils prettyprinter process SHA text
          time utf8-string word-wrap yaml
        ];
        testHaskellDepends = [ base containers HUnit ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      cabal = pkgs.haskellPackages.cabalNoTest;
      parameterized-utils = self.callCabal2nix "parameterized-utils" (pkgs.fetchFromGitHub {
        owner = "GaloisInc";
        repo = "parameterized-utils";
        rev = "v2.0";
        sha256 = "1cb3rz3v0ggaiqn3qr2j9x5ws22mxqkp091ibrz8s0h792n8cx35";
      }) {};
    };
  };

  drv = haskellPackages.callPackage f {};

in

  pkgs.lib.overrideDerivation drv.env (old: {
   buildInputs = old.buildInputs ++ [ pkgs.graphviz ];
  })
# >>>>>>> master
