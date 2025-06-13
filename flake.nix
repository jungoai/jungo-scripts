{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      hPkgs = pkgs.haskell.packages."ghc984";
    in
    {
      devShells.default = pkgs.mkShell {
        packages = [
          hPkgs.ghc
          hPkgs.hoogle
          hPkgs.haskell-language-server
          hPkgs.cabal-install

          pkgs.zlib
          pkgs.pkg-config # need to find zlib
        ];
      };
    });
}
