{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
    opam-repository = {
      url = "github:aprove-developers/opam-repository";
      flake = false;
    };
  };

  outputs =
    {
      self,
      flake-utils,
      opam-nix,
      opam-repository,
      nixpkgs,
      ...
    }:
    let
      package = "koat2";
      ocamlVersion = "5.5.0";
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (pkgs) lib;
        musl = pkgs.pkgsCross.musl64;
        on = opam-nix.lib.${system};

        # Only what the opam/dune build needs, so that changes to the flake,
        # examples, Docker files etc. don't change koat2's source (and thus
        # don't trigger rebuilds). The files may be missing, so that the
        # dependencies can be built from just the flake and the .opam files
        # (see the Dockerfile).
        mkSrc =
          extraFiles:
          lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions (
              map lib.fileset.maybeMissing (
                [
                  ./bin
                  ./src
                  ./test
                  ./external
                  ./dune
                  ./dune-project
                  ./koat2.opam
                  ./koat2_external.opam
                  # Installed as documentation by dune.
                  ./LICENSE
                  ./README.md
                ]
                ++ extraFiles
              )
            );
          };
        src = mkSrc [ ];
        # The static build runs the tests, which read these examples.
        srcWithTestExamples = mkSrc [
          ./examples/CageKoAT-Input-Examples
          ./examples/KoAT-2013
          ./examples/KoAT-2014
          ./examples/SAS10
          ./examples/T2
          ./examples/probabilistic
          ./examples/rec
        ];

        # opam-nix resolves the dependencies from a local opam repository built
        # from the project directory it is given. Giving it only the .opam
        # files keeps the resolution (done while evaluating the flake) from
        # being redone after every source edit; the local packages get their
        # actual sources via `localSourcesOverlay`.
        opamFiles = lib.fileset.toSource {
          root = ./.;
          fileset = lib.fileset.unions [
            ./koat2.opam
            ./koat2_external.opam
          ];
        };
        localPackages = [
          package
          "koat2_external"
        ];

        devPackagesQuery = {
          ocaml-lsp-server = "*";
          # `ocamlmerlin`, used by Emacs' merlin-mode (e.g. Doom's ocaml module
          # without +lsp).
          merlin = "*";
          # For `dune utop`.
          utop = "*";
        };
        query = devPackagesQuery // {
          # Flambda compiler.
          ocaml-variants = "${ocamlVersion}+options";
          ocaml-option-flambda = "*";
          # Since 5.5 the compiler's build script otherwise asks opam for
          # existing switches to clone the compiler from.
          compiler-cloning = "disabled";
        };

        # --- Helpers for overriding opam-nix packages -----------------------

        # Overlay applying `f` to every opam package of a scope (leaving the
        # scope's helper attributes alone).
        mapOpamPackages =
          f: final: prev:
          lib.mapAttrs (_: p: if lib.isDerivation p && p ? passthru.pkgdef then f prev p else p) prev;

        # opam-nix generates a package's build and install phases from its
        # parsed opam file in `passthru.pkgdef`, so editing that is the least
        # invasive way to change what opam would run.
        overridePkgdef =
          p: f:
          p.overrideAttrs (oa: {
            passthru = oa.passthru // {
              pkgdef = f oa.passthru.pkgdef;
            };
          });

        # opam-nix doesn't implement opam's `%{pkg:installed?then:else}%`
        # interpolation and turns it into a failing shell expression. Whether
        # a package is installed is just whether it is in the resolved
        # package set, so resolve these before opam-nix sees them.
        resolveInstalledTernaries =
          scope: p:
          let
            ternary = "%[{]([^:}]+):installed[?]([^:}]*):([^}]*)[}]%";
            resolve = lib.concatMapStrings (
              piece:
              if lib.isList piece then
                (if scope ? ${lib.elemAt piece 0} then lib.elemAt piece 1 else lib.elemAt piece 2)
              else
                piece
            );
            # Arguments are strings or `{ val; conditions; }` for filtered ones.
            go =
              v:
              if lib.isString v then
                resolve (builtins.split ternary v)
              else if lib.isList v then
                map go v
              else if v ? val then
                v // { val = go v.val; }
              else
                v;
            commands = pkgdef: [
              (pkgdef.build or [ ])
              (pkgdef.install or [ ])
            ];
          in
          if builtins.match ".*${ternary}.*" (builtins.toJSON (commands p.passthru.pkgdef)) == null then
            p
          else
            overridePkgdef p (
              pkgdef:
              pkgdef
              // lib.optionalAttrs (pkgdef ? build) { build = go pkgdef.build; }
              // lib.optionalAttrs (pkgdef ? install) { install = go pkgdef.install; }
            );

        # --- Overrides for both builds --------------------------------------

        # The local packages' sources (see `opamFiles`).
        localSourcesOverlay =
          src: final: prev:
          lib.genAttrs localPackages (name: overridePkgdef prev.${name} (pkgdef: pkgdef // { inherit src; }));

        # Shown by `koat2 --version`, like the version string the Dockerfile
        # sets. Only koat2 itself depends on the git revision, which is unknown
        # outside of a git checkout (e.g., in the Dockerfile).
        gitRevision = self.shortRev or self.dirtyShortRev or null;
        versionOverlay =
          final: prev:
          lib.optionalAttrs (gitRevision != null) {
            ${package} = prev.${package}.overrideAttrs (oa: {
              postPatch = (oa.postPatch or "") + ''
                substituteInPlace src/util/VersionString.ml \
                  --replace-fail 'version_str = "VERSION_STR"' 'version_str = "${gitRevision}"'
              '';
            });
          };

        commonOverlay = lib.composeExtensions 
          (mapOpamPackages resolveInstalledTernaries)
          (
            final: prev:
            {
              # Their probe searches fixed prefixes (/usr/local, …) for the
              # headers; Nix provides gmp/mpfr to the dependent builds directly.
              conf-gmp-paths = prev.conf-gmp-paths.overrideAttrs { dontBuild = true; };
              conf-mpfr-paths = prev.conf-mpfr-paths.overrideAttrs { dontBuild = true; };
            }
            // lib.optionalAttrs (!(prev ? ocaml-config)) {
              # opam-nix points `ocaml` at the `ocaml-config` package, which
              # `ocaml` no longer depends on since 5.5.
              ocaml = prev.ocaml.overrideAttrs { opam__ocaml_config__share = ""; };
            }
          )
        ;

        # --- Regular (dynamically linked) build -----------------------------

        # opam builds with `dune build -p`, which implies the release profile,
        # in which this project links statically. Keep opam's build, but in
        # the dev profile, like a plain `dune build`.
        devProfileOverlay = final: prev: {
          ${package} = overridePkgdef prev.${package} (
            pkgdef:
            let
              isDuneBuild = cmd: lib.isList cmd && lib.take 2 cmd == [ "dune" "build" ];
            in
            lib.throwIfNot (lib.any isDuneBuild pkgdef.build) "${package}.opam has no `dune build` command" (
              pkgdef
              // {
                build = map (cmd: if isDuneBuild cmd then cmd ++ [ "--profile" "dev" ] else cmd) pkgdef.build;
              }
            )
          );
        };

        scope =
          (on.buildOpamProject { repos = [ opam-repository ]; } package opamFiles query).overrideScope
            (
              lib.composeManyExtensions [
                (localSourcesOverlay src)
                versionOverlay
                commonOverlay
                devProfileOverlay
              ]
            );

        # --- Fully static build ---------------------------------------------

        # The released binary (also built by the Dockerfile behind
        # compile_static_binary.sh): an ordinary musl toolchain, with the static
        # link coming only from the project's release profile
        # (`-ccopt -static`). `pkgsStatic` doesn't work because it disables
        # dynamic loading in the whole toolchain, which breaks dune's
        # bootstrap; linking statically against glibc fails on TLS symbols
        # from C stubs.

        # The musl cross toolchain only has prefixed tools
        # (x86_64-unknown-linux-musl-gcc, …), but several opam build scripts
        # call the plain names, as they would on Alpine.
        muslToolAliases = pkgs.symlinkJoin {
          name = "musl-tool-aliases";
          paths = map (
            tool: pkgs.writeShellScriptBin tool ''exec ${musl.stdenv.cc.targetPrefix}${tool} "$@"''
          ) [ "cc" "c++" "cpp" "ar" "ranlib" "ld" "strip" "nm" ];
        };

        staticOverlay = lib.composeManyExtensions [
          (mapOpamPackages (
            _: p:
            p.overrideAttrs (oa: {
              nativeBuildInputs = (oa.nativeBuildInputs or [ ]) ++ [ muslToolAliases ];
            })
          ))
          (final: prev: {
            ${package} = prev.${package}.overrideAttrs (oa: {
              # nixpkgs builds these shared-only by default; the static link
              # needs their `.a` archives.
              buildInputs = oa.buildInputs ++ [
                (musl.gmp.override { withStatic = true; })
                (musl.mpfr.overrideAttrs { dontDisableStatic = true; })
                (musl.ppl.overrideAttrs { dontDisableStatic = true; })
                # Linked in the release profile, see ./dune. With musl, mimalloc's
                # <linux/prctl.h> conflicts with <sys/prctl.h>, which suffices.
                (musl.mimalloc.overrideAttrs (oa: {
                  postPatch = (oa.postPatch or "") + ''
                    substituteInPlace src/prim/unix/prim.c --replace-fail "#include <linux/prctl.h>" ""
                  '';
                }))
              ];
              # Runs the tests (opam's `{with-test}`), as this is the released binary.
              doCheck = true;
              # The released binary is stripped.
              postFixup = ''
                $STRIP $out/bin/${package}
              '';
            });
          })
        ];

        scopeStatic =
          (on.buildOpamProject {
            pkgs = musl;
            repos = [ opam-repository ];
          } package opamFiles query).overrideScope
            (
              lib.composeManyExtensions [
                (localSourcesOverlay srcWithTestExamples)
                versionOverlay
                commonOverlay
                staticOverlay
              ]
            );

        koat2 = scope.${package};
        koat2Static = scopeStatic.${package};

        devPackages = builtins.attrValues (lib.getAttrs (builtins.attrNames devPackagesQuery) scope);

        # The shells take their inputs from the packages, but dune builds the
        # local packages from the checkout there, so leave out the Nix-built
        # ones; otherwise every source edit would rebuild them just to enter
        # the shell.
        withoutLocalPackages =
          drv:
          let
            isLocal = d: lib.isDerivation d && lib.elem (d.passthru.pkgdef.name or null) localPackages;
          in
          drv.overrideAttrs (
            oa:
            lib.genAttrs [ "buildInputs" "nativeBuildInputs" "propagatedBuildInputs" ] (
              attr: lib.filter (d: !isLocal d) (oa.${attr} or [ ])
            )
          );

        # opam-nix propagates a fake `opam` (for opam build scripts) through
        # every package, so the shells would have ~100 of them on PATH. Drop
        # them, so tools looking for a real opam (e.g. Emacs' opam-switch-mode)
        # don't pick up one that can't answer `opam var switch`.
        hideFakeOpam = ''
          PATH=$(tr ':' '\n' <<<"$PATH" | while read -r dir; do
            grep -qs "opam-nix fake opam" "$dir/opam" || echo "$dir"
          done | paste -sd:)
        '';
      in
      {
        packages = {
          default = koat2;
          koat2 = koat2;
          koat2-static = koat2Static;
        };

        apps = {
          default = flake-utils.lib.mkApp {
            drv = koat2;
            exePath = "/bin/koat2";
          };
          koat2-static = flake-utils.lib.mkApp {
            drv = koat2Static;
            exePath = "/bin/koat2";
          };
        };

        devShells = {
          default = pkgs.mkShell {
            inputsFrom = [ (withoutLocalPackages koat2) ];
            buildInputs = devPackages;
            shellHook = hideFakeOpam;
          };
          # The static package's build environment: `dune build --release`
          # here produces the fully static binary.
          staticDev = musl.mkShell {
            inputsFrom = [ (withoutLocalPackages koat2Static) ];
            shellHook = hideFakeOpam;
          };
        };
      }
    );
}
