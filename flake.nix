{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
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
        # The nixpkgs `sbt` package with `sbt` replaced by the BSP shim above. Its `sbtn` (the thin
        # client) is the binary the launcher itself runs for a plain `sbt` on an sbt 2 build;
        # `--server` runs sbt in-process instead.
        sbtWithBspShim = pkgs.symlinkJoin {
          name = "sbt-with-bsp-shim";
          paths = [ sbt0 ];
          postBuild = ''
            rm -f $out/bin/sbt
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
              sbtWithBspShim
            ];
            inherit (pre-commit-check) shellHook;
          };
          # What the CI workflow runs (`nix develop .#ci`): only what the recipes need, with no
          # developer tools and no pre-commit hook installed on entry.
          ci = pkgs.mkShell {
            JAVA_OPTS = "-Xmx4g -Xss512m -XX:+UseG1GC";
            packages = with pkgs; [
              jdk
              just
              nixfmt
              sbtWithBspShim
            ];
          };
        };
      }
    ));
}
