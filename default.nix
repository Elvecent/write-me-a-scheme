{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, cabal-install, genvalidity, validity
      , haskeline, megaparsec, prettyprinter, QuickCheck, stdenv
      , stylish-haskell, tasty, tasty-quickcheck, text
      }:
      mkDerivation {
        pname = "write-me-a-scheme";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base haskeline megaparsec prettyprinter QuickCheck
          tasty tasty-quickcheck text validity genvalidity
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
