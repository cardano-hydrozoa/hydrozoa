{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, flake-utils, nixpkgs, git-hooks, ... }@inputs:
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        jdk = pkgs.openjdk25;
        # GraalVM is an advanced JDK. We use it for  metals and bloop only.
        # https://www.graalvm.org/
        sbt0 = pkgs.sbt.override { jre = jdk; };
        visualvm = pkgs.visualvm.override { jdk = jdk; };
        # Define the hooks
        pre-commit-check = git-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            precommit = {
              enable = true;
              name = "lint fmt check";
              entry = "${pkgs.just}/bin/just precommit";
              pass_filenames = false;
            };
          };
        };
      in rec {
        devShell = pkgs.mkShell {
          JAVA_OPTS = "-Xmx4g -Xss512m -XX:+UseG1GC";
          # This fixes bash prompt/autocomplete issues with subshells (i.e. in VSCode) under `nix develop`/direnv
          buildInputs = [ pkgs.bashInteractive ];
          packages = with pkgs; [
            ammonite # modernized scala repl: https://ammonite.io/
            async-profiler # Low-overhead profiler for the JVM: https://github.com/async-profiler/async-profiler
            bloop # faster incremental compilation
            jdk
            just # command runner, similar to `make`
            ltex-ls # Language server for markdown: https://github.com/valentjn/ltex-ls
            nixfmt
            sbt0
            scala-cli
            scalafix
            scalafmt
            # Visualize programs running on the JVM. May need _JAVA_AWT_WM_NONREPARENTING=1 on wayland:
            #    https://github.com/oracle/visualvm/issues/403		 
            visualvm
          ];
          inherit (pre-commit-check) shellHook;
        };
      }));
}
