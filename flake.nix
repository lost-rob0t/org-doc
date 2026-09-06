{
  description = "org-doc — Org-mode API documentation from Common Lisp docstrings, with SLY/SLIME live docs for Emacs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        org-doc-lib = pkgs.sbcl.buildASDFSystem rec {
          pname = "org-doc";
          version = "0.1.0";
          lispSystems = [ "org-doc" ];
          src = ./.;
          lispLibs = [ ];
        };

        sbcl' = pkgs.sbcl.withOverrides (self: super: {
          inherit org-doc-lib;
        });

        sbcl-with-org-doc = sbcl'.withPackages (ps: with ps; [ org-doc-lib ]);

        # CLI: `org-doc --system SYSTEM --out doc/api/`
        org-doc-bin = pkgs.writeShellApplication {
          name = "org-doc";
          runtimeInputs = [ sbcl-with-org-doc ];
          text = ''
            exec sbcl --non-interactive --no-userinit --no-sysinit \
              --eval "(require :asdf)" \
              --eval "(asdf:load-system :org-doc/cli)" \
              --eval "(org-doc/cli:main)" "$@"
          '';
        };

        # Test runner over the flake source
        org-doc-test-runner = pkgs.writeShellApplication {
          name = "org-doc-tests";
          runtimeInputs = [ sbcl-with-org-doc ];
          text = ''
            exec sbcl --non-interactive --no-userinit --no-sysinit \
              --eval "(require :asdf)" \
              --eval "(asdf:load-asd #p\"${./org-doc.asd}\")" \
              --eval "(asdf:load-system :org-doc/tests :force t)" \
              --eval "(org-doc/tests:run)"
          '';
        };

        emacsPackage = pkgs.emacsPackages.trivialBuild {
          pname = "sly-org-doc";
          version = "0.1.0";
          src = ./elisp;
        };
      in
      {
        packages = {
          inherit org-doc-bin org-doc-test-runner emacsPackage;
          default = org-doc-bin;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            sbcl-with-org-doc
            org-doc-bin
            pkgs.emacs
            emacsPackage
          ];
        };

        checks = {
          inherit org-doc-test-runner;
        };
      });
}
