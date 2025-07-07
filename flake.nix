{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      hPkgs = pkgs.haskell.packages."ghc984";

      jungochainRunPkg = hPkgs.callCabal2nix "jungochain-run" ./. { };
    in
    {
      devShells.default = pkgs.mkShell {
        packages = [
          hPkgs.ghc
          hPkgs.hoogle
          hPkgs.haskell-language-server
          hPkgs.cabal-install

          pkgs.zlib
          pkgs.pkg-config # need for finding zlib
        ];
      };

      packages.default = jungochainRunPkg;

      # Define a `nix run` app (if your project produces an executable)
      apps.default = {
        type = "app";
        program = "${jungochainRunPkg}/bin/jungochain-run"; # Replace with your executable name
      };
    });
}
