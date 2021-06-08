{ pkgs ? import <nixpkgs> {}}:
with pkgs;
let
  my-ghc = haskellPackages.ghcWithPackages (h: [ h.zlib h.vector ]);
in
mkShell {
  buildInputs = [ my-ghc ];
}
