{
  description = "Ben Lynn's neural network from scratch in Haskell";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  # train-images-idx3-ubyte.gz https://web.archive.org/web/20210304042348/http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz
  # train-labels-idx1-ubyte.gz https://web.archive.org/web/20201031192846/http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz
  # t10k-images-idx3-ubyte.gz  https://web.archive.org/web/20200802112939/http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz
  # t10k-labels-idx1-ubyte.gz  https://web.archive.org/web/20210124213857/http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      with import nixpkgs { inherit system; }; {
        defaultPackage =
          let
            train-images-idx3-ubyte = fetchurl {
              url = "https://web.archive.org/web/20210304042348/http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz";
              sha256 = "sha256-RA/Kv3PMVG+iFHXoHqNwJlYF9WviEKQCTSyo8gNSNgk=";
            };
            train-labels-idx1-ubyte = fetchurl {
              url = "https://web.archive.org/web/20201031192846/http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz";
              sha256 = "sha256-NVJTSgpVi77WrtMrMMSVzKI9Vn7FLKyL4aBzDoAQJVw=";
            };
            t10k-images-idx3-ubyte = fetchurl {
              url = "https://web.archive.org/web/20200802112939/http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz";
              sha256 = "sha256-jUIsewocHHkkWlvPB/6G4z7q/ueSuEWErsJ29aLbxOY=";
            };
            t10k-labels-idx1-ubyte = fetchurl {
              url = "https://web.archive.org/web/20210124213857/http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz";
              sha256 = "sha256-965g+S4A7G3r0jpgiMMdvSNx7KP/oN7677JZkkIErsY=";
            };
          in
          stdenv.mkDerivation {
            name = "morans";
            src = ./.;
            nativeBuildInputs = [ (haskellPackages.ghcWithPackages (h: [ h.zlib h.random ])) ];
            buildPhase = ''
              ghc -O3 morans.hs -o morans
            '';
            installPhase = ''
              mkdir -p $out
              cp morans $out
              cp ${train-images-idx3-ubyte} $out/train-images-idx3-ubyte.gz
              cp ${train-labels-idx1-ubyte} $out/train-labels-idx1-ubyte.gz
              cp ${t10k-images-idx3-ubyte} $out/t10k-images-idx3-ubyte.gz
              cp ${t10k-labels-idx1-ubyte} $out/t10k-labels-idx1-ubyte.gz
            '';
          };
        devShell = import ./shell.nix { inherit pkgs; };
      }
    );
}
