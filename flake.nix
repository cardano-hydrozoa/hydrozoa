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
      {
        devShells = {
          default = pkgs.mkShell {
            JAVA_OPTS = "-Xmx4g -Xss512m -XX:+UseG1GC";
            # This fixes bash prompt/autocomplete issues with subshells (i.e. in VSCode) under `nix develop`/direnv
            buildInputs = [ pkgs.bashInteractive ];
            packages = with pkgs; [
              git # otherwise `git` resolves to the broken macOS Xcode shim inside `nix develop`
              jdk
              just # command runner, similar to `make`
              libnotify # used in justfile
              nixfmt
              sbtNoSbtn
            ];
            inherit (pre-commit-check) shellHook;
          };
          # What the CI workflow runs (`nix develop .#ci`): the JDK, sbt, and the recipes' `just` and
          # `nixfmt`. No developer tools, and no pre-commit hook installed on entry.
          ci = pkgs.mkShell {
            JAVA_OPTS = "-Xmx4g -Xss512m -XX:+UseG1GC";
            packages = with pkgs; [
              jdk
              just
              nixfmt
              sbtNoSbtn
            ];
          };
        };
      }
    ));
}
