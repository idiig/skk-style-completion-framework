{
  description = "SKK-style completion framework and pyim integration for Emacs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    inputs@{ flake-parts, nixpkgs, ... }:
    let
      version = "0.1.0";

      # Only skk-style-completion-framework.el -- the generic 3-stage
      # (continuation/abbrev/convert) protocol itself, no input-method
      # dependency.  Anything implementing the same protocol (a future
      # SKK/wubi backend, say) can depend on just this instead of
      # dragging in pyim.
      mkFrameworkPackage =
        emacsPackages:
        emacsPackages.trivialBuild {
          pname = "skk-style-completion-framework";
          inherit version;
          src = builtins.path {
            path = ./.;
            name = "skk-style-completion-framework-src";
            filter = path: type: type == "directory" || baseNameOf path == "skk-style-completion-framework.el";
          };
          packageRequires = [ ];

          meta = {
            description = "Generic 3-stage (continuation/abbrev/convert) compose-completion protocol";
            license = nixpkgs.lib.licenses.gpl3Plus;
            platforms = nixpkgs.lib.platforms.all;
          };
        };

      # Only pyim-skk-style.el -- pyim's own implementation of that
      # protocol (SKK-style shifted start keys, completion preview,
      # candidate confirmation).  Depends on the framework package
      # above rather than bundling a second copy of its source.
      mkPyimPackage =
        emacsPackages:
        emacsPackages.trivialBuild {
          pname = "pyim-skk-style";
          inherit version;
          src = builtins.path {
            path = ./.;
            name = "pyim-skk-style-src";
            filter = path: type: type == "directory" || baseNameOf path == "pyim-skk-style.el";
          };
          packageRequires = [
            (mkFrameworkPackage emacsPackages)
            emacsPackages.pyim
          ];

          meta = {
            description = "SKK-style pyim integration on top of skk-style-completion-framework";
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
        lib = {
          mkFrameworkPackage = { emacsPackages }: mkFrameworkPackage emacsPackages;
          mkPyimPackage = { emacsPackages }: mkPyimPackage emacsPackages;
        };

        emacsOverlays.default = final: _prev: {
          skk-style-completion-framework = mkFrameworkPackage final;
          pyim-skk-style = mkPyimPackage final;
        };

        flakeModules.default = { ... }: {
          perSystem =
            { pkgs, ... }:
            {
              packages.skk-style-completion-framework = mkFrameworkPackage pkgs.emacsPackages;
              packages.pyim-skk-style = mkPyimPackage pkgs.emacsPackages;
            };
        };
      };

      perSystem =
        { pkgs, ... }:
        let
          framework = mkFrameworkPackage pkgs.emacsPackages;
          pyimIntegration = mkPyimPackage pkgs.emacsPackages;
        in
        {
          packages.default = pyimIntegration;
          packages.skk-style-completion-framework = framework;
          packages.pyim-skk-style = pyimIntegration;
          checks.default = pyimIntegration;
        };
    };
}
