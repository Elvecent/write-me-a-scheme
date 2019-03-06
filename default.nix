{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, cabal-install, generic-random
      , haskeline, megaparsec, prettyprinter, QuickCheck, stdenv
      , stylish-haskell, tasty, text
      }:
      mkDerivation {
        pname = "write-me-a-scheme";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base generic-random haskeline megaparsec prettyprinter QuickCheck
          tasty text
        ];
        executableSystemDepends = [ cabal-install stylish-haskell ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
