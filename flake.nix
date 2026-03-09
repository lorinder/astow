{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          basePackages = pkgs.haskellPackages;
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.astow.overrideAttrs (old: {
          postInstall = (old.postInstall or "") + ''
            install -Dm644 ${./doc/astow.1} $out/share/man/man1/astow.1
          '';
        });
      };
    };
}
