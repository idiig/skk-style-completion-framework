{
  description = "SKK-style completion framework and pyim integration for Emacs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    inputs@{ flake-parts, nixpkgs, ... }:
    let
      pname = "skk-style-completion-framework";
      version = "0.1.0";

      mkPackage =
        emacsPackages:
        emacsPackages.trivialBuild {
          inherit pname version;
          src = ./.;
          packageRequires = [ emacsPackages.pyim ];

          meta = {
            description = "Generic 3-stage compose-completion protocol with pyim SKK-style integration";
            license = nixpkgs.lib.licenses.gpl3Plus;
            platforms = nixpkgs.lib.platforms.all;
          };
        };
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      flake = {
        lib.mkPackage = { emacsPackages }: mkPackage emacsPackages;

        emacsOverlays.default = final: _prev: {
          skk-style-completion-framework = mkPackage final;
        };

        flakeModules.default = { ... }: {
          perSystem =
            { pkgs, ... }:
            {
              packages.skk-style-completion-framework = mkPackage pkgs.emacsPackages;
            };
        };
      };

      perSystem =
        { pkgs, ... }:
        let
          package = mkPackage pkgs.emacsPackages;
        in
        {
          packages.default = package;
          packages.skk-style-completion-framework = package;
          checks.default = package;
        };
    };
}
