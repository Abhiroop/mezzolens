{ pkgs ? import <nixpkgs> {}
, hp ? pkgs.haskellPackages
}:

hp.callPackage
 ({ mkDerivation, transformers, mtl }:
  mkDerivation {
   pname = "mezzolens";
   version = "0.0.0";
   src = pkgs.lib.sourceFilesBySuffices ./. [".hs" ".cabal" "CHANGELOG" "LICENSE"];
   buildDepends = [ transformers mtl ];
   license = pkgs.lib.licenses.asl20;
 }) {}
