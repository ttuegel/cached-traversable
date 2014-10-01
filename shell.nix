{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages
}:

haskellPackages.callPackage ./. {}
