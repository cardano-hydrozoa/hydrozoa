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
        # NB: the bundled `sbtn` thin client is sbt 1.x and cannot drive an sbt 2 server
        # (it reports `unknown event: sbt/exec`); use `sbt`, not `sbtn`, until nixpkgs ships sbt 2.
        sbt0 = pkgs.sbt.override { jre = jdk; };
        visualvm = pkgs.visualvm.override { jdk = jdk; };
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
            ammonite # modernized scala repl: https://ammonite.io/
            async-profiler # Low-overhead profiler for the JVM: https://github.com/async-profiler/async-profiler
            jdk
            just # command runner, similar to `make`
            libnotify # used in justfile
            ltex-ls # Language server for markdown: https://github.com/valentjn/ltex-ls
            nixfmt
            sbt0
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
      }
    ));
}
