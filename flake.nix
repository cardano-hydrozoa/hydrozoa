{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , ...
    } @ inputs:
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        jdk = pkgs.openjdk23;
        graalvm = pkgs.graalvmPackages.graalvm-ce;
        metals0 = pkgs.metals.override { jre = graalvm; };
        bloop0 = pkgs.bloop.override { jre = graalvm; };
        sbt0 = pkgs.sbt.override { jre = jdk; };
        visualvm = pkgs.visualvm.override { jdk = jdk; };
      in
      rec {
        devShell = pkgs.mkShell {
          JAVA_HOME = "${graalvm}";
          JAVA_OPTS = "-Xmx4g -Xss512m -XX:+UseG1GC";
          # This fixes bash prompt/autocomplete issues with subshells (i.e. in VSCode) under `nix develop`/direnv
          buildInputs = [ pkgs.bashInteractive ];
          packages = with pkgs; [
            ammonite
            async-profiler
            bloop0
            just
            jdk
            metals0
            sbt0
            scala-cli
            scalafix
            scalafmt
     	      visualvm
          ];
        };
      })
    );
}
