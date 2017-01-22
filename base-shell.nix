with import <nixpkgs> {};
with haskellPackages;

stdenv.mkDerivation {
  name = "sat-monad";
  buildInputs = [ ghc cabal2nix cabal-install styx ];
}
