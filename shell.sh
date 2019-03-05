nix-shell -p 'haskell.packages.ghc863.ghcWithPackages(p: with p; [ megaparsec text QuickCheck haskeline stylish-haskell prettyprinter ])'
