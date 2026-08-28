{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      flake-utils,
      nixpkgs,
      git-hooks,
      ...
    }@inputs:
    (flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        jdk = pkgs.openjdk25;
        # The nixpkgs sbt launcher (1.x) reads project/build.properties and bootstraps whatever
        # sbt version it names — including sbt 2.x — so no launcher pin is needed here.
        sbt0 = pkgs.sbt.override { jre = jdk; };
        # sbt 2's `bspConfig` writes `.bsp/sbt.json` with argv `[<sbt>, "bsp"]`, but the nixpkgs
        # launcher script has no `bsp` handling and sbt 2 has no `bsp` command, so IDEA's BSP sync
        # (`sbt bsp`) dies on startup and the import times out. sbt only starts its BSP server via
        # the `-bsp` launcher flag, so translate a bare `bsp` arg to `-bsp`. Pin `sbt.script` to
        # this wrapper so `bspConfig` records the wrapper (not the inner launcher) in .bsp/sbt.json.
        sbtBspShim = pkgs.writeShellScriptBin "sbt" ''
          self="$(readlink -f "$0")"
          args=()
          for a in "$@"; do
            [ "$a" = "bsp" ] && a="-bsp"
            args+=("$a")
          done
          exec ${sbt0}/bin/sbt "-Dsbt.script=$self" "''${args[@]}"
        '';
        # The nixpkgs `sbt` package also bundles the `sbtn` thin client (sbt 1.x), which cannot
        # drive an sbt 2 server (it reports `unknown event: sbt/exec`). Strip it so only `sbt` is on
        # PATH — nobody should reach for the broken client by habit. Restore once nixpkgs ships an
        # sbt 2 `sbtn`. `sbt` itself is the BSP shim above.
        sbtNoSbtn = pkgs.symlinkJoin {
          name = "sbt-no-sbtn";
          paths = [ sbt0 ];
          postBuild = ''
            rm -f $out/bin/sbtn $out/bin/sbt
            ln -s ${sbtBspShim}/bin/sbt $out/bin/sbt
          '';
        };
        visualvm = pkgs.visualvm.override { jdk = jdk; };
        # ── Canton (Digital Asset) ─────────────────────────────────────────────
        # Canton open-source runtime: participant / sequencer / mediator nodes +
        # the Canton console. Not in nixpkgs, so fetch the release tarball and wrap
        # the sbt-native-packager launcher with a pinned LTS JDK. The devShell JDK
        # is 25 (for the Scala build); Canton 3.x targets Java 17–21, so pin 21 here
        # independently rather than risk the JVM the build happens to use.
        cantonVersion = "3.5.15";
        cantonJdk = pkgs.openjdk21;
        canton = pkgs.stdenv.mkDerivation {
          pname = "canton";
          version = cantonVersion;
          src = pkgs.fetchurl {
            url = "https://github.com/digital-asset/canton/releases/download/v${cantonVersion}/canton-open-source-${cantonVersion}.tar.gz";
            hash = "sha256-oRRT2YkXvmE2yy6qP4APW2fuWez/dZB/e1m0zP03Zrg=";
          };
          nativeBuildInputs = [ pkgs.makeWrapper ];
          dontConfigure = true;
          dontBuild = true;
          installPhase = ''
            runHook preInstall
            mkdir -p $out/libexec/canton $out/bin
            cp -r . $out/libexec/canton/
            makeWrapper $out/libexec/canton/bin/canton $out/bin/canton \
              --set JAVA_HOME ${cantonJdk} \
              --prefix PATH : ${cantonJdk}/bin
            runHook postInstall
          '';
          meta = {
            description = "Canton open-source runtime: sequencer/mediator (synchronizer) + participant nodes + console";
            homepage = "https://www.canton.network";
          };
        };
        # ── Daml SDK — TODO (canton-hydrozoa) ──────────────────────────────────
        # The Daml 3.x SDK/compiler (Daml-LF 2.x, which Canton 3.5 runs) is NOT a
        # public GitHub release: digital-asset/daml tops out at v2.10.6 (LF 1.x,
        # the Daml 2.x line) and v3.x tags 404 — the 3.x compiler ships via
        # `daml install <ver>` from get.daml.com / Artifactory. Pinning it needs
        # (a) the exact 3.x version matching Canton 3.5.15 and (b) a fetchable
        # source + hash. Deferred: PoC DAML template authoring picks this up next.
        # (The public 2.10.6 SDK compiles only LF 1.x, which Canton 3.5 will likely
        # reject, so it is intentionally NOT wired in as a stopgap.)
        # Define the hooks
        pre-commit-check = git-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            precommit = {
              enable = true;
              name = "lint fmt check";
              # sbt 2 concatenates multiple program args into one command line, so pass a single
              # `;`-separated command instead of two args (`"scalafixAll --check" scalafmtCheck`).
              entry = "${pkgs.bash}/bin/bash -c '${sbt0}/bin/sbt \"; scalafixAll --check ; scalafmtCheck\" && ${pkgs.nixfmt}/bin/nixfmt flake.nix --check'";
              pass_filenames = false;
            };
          };
        };
      in
      rec {
        devShell = pkgs.mkShell {
          JAVA_OPTS = "-Xmx4g -Xss512m -XX:+UseG1GC";
          # This fixes bash prompt/autocomplete issues with subshells (i.e. in VSCode) under `nix develop`/direnv
          buildInputs = [ pkgs.bashInteractive ];
          packages = with pkgs; [
            canton # Digital Asset Canton runtime (nodes + console); see let-binding above
            ammonite # modernized scala repl: https://ammonite.io/
            async-profiler # Low-overhead profiler for the JVM: https://github.com/async-profiler/async-profiler
            git # otherwise `git` resolves to the broken macOS Xcode shim inside `nix develop`
            jdk
            just # command runner, similar to `make`
            libnotify # used in justfile
            ltex-ls # Language server for markdown: https://github.com/valentjn/ltex-ls
            nixfmt
            sbtNoSbtn
            scala-cli
            scalafix
            scalafmt
            # Visualize programs running on the JVM. May need _JAVA_AWT_WM_NONREPARENTING=1 on wayland:
            #    https://github.com/oracle/visualvm/issues/403
            visualvm
            nodejs_24 # this is needed by IDEA's MCP Server
            mermaid-cli
          ];
          inherit (pre-commit-check) shellHook;
        };
        packages.canton = canton;
      }
    ));
}
