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

            install -Dm644 /dev/null $out/share/bash-completion/completions/astow
            $out/bin/astow --bash-completion-script $out/bin/astow \
              > $out/share/bash-completion/completions/astow

            install -Dm644 /dev/null $out/share/zsh/site-functions/_astow
            $out/bin/astow --zsh-completion-script $out/bin/astow \
              > $out/share/zsh/site-functions/_astow

            install -Dm644 /dev/null $out/share/fish/vendor_completions.d/astow.fish
            $out/bin/astow --fish-completion-script $out/bin/astow \
              > $out/share/fish/vendor_completions.d/astow.fish
          '';
        });

        # Fully static binary via musl.  Build with: nix build .#astow-static
        packages.astow-static =
          let
            hp = pkgs.pkgsStatic.haskellPackages;
          in
            pkgs.haskell.lib.compose.justStaticExecutables
              (pkgs.haskell.lib.compose.appendConfigureFlags [
                "--ghc-option=-optl=-static"
                "--ghc-option=-optl=-pthread"
              ] (hp.callCabal2nix "astow" ./. { }));
      };
    };
}
