{
  description = "A library for type-level programming.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nix-filter.url = "github:numtide/nix-filter/main";
  };

  outputs = { self, nixpkgs, nix-filter }: 
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };

      # filter = import nix-filter { };

      ghcVersion = "925";
      # ghcVersion = "92";
      # ghcVersion = "902";

      src = nix-filter.lib {
        root = ./.;
        include = [
          (nix-filter.lib.inDirectory "src")
          (nix-filter.lib.inDirectory "examples")
          (nix-filter.lib.inDirectory "test")
          (nix-filter.lib.matchExt "hs")
          ./first-class-families.cabal
          ./cabal.project
          ./LICENSE
        ];
      };

      first-class-families = hself: hself.callCabal2nix "first-class-families" src {};

      myHaskellPackages = pkgs.haskell.packages."ghc${ghcVersion}".override {
        overrides = hself: hsuper: {
          first-class-families = first-class-families hself;
          # ListLike = pkgs.haskell.lib.dontCheck hsuper.ListLike;
          # type-of-html = pkgs.haskell.lib.doBenchmark (hself.callPackage ./nix/type-of-html.nix {inherit src;});
          # type-of-html = hself.callCabal2nix "type-of-html" src {};
          # optics-core = hsuper.optics-core.overrideAttrs(old: {
          #   configureFlags = "-f explicit-generic-labels";
          #   patches = [./optics-core.patch];
          # });
        };
      };

      shell = myHaskellPackages.shellFor {
        packages = p: [
          p.first-class-families
        ];
        buildInputs = with pkgs.haskell.packages."ghc${ghcVersion}"; [
          myHaskellPackages.cabal-install
          ghcid
          (pkgs.haskell-language-server.override { supportedGhcVersions = [ "${ghcVersion}" ]; })
          hlint
          # implicit-hie
          # cabal2nix
        ];
        withHoogle = true;
        doBenchmark = true;
      };

    in
      {
        library = first-class-families;
        # packages.x86_64-linux.default = ;
        devShell.x86_64-linux = shell;
        inherit pkgs;
      };
}
