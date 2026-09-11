# Releasing Hydrozoa

A release publishes the `hydrozoa` Docker image to the GitHub Container Registry
(`ghcr.io/cardano-hydrozoa/hydrozoa`). The image bundles the whole CLI — the head-node server
(`serve`) and every deployment command — so it is the single artifact a deployer needs (see
[DEPLOYMENT.md](docs/user-guide/DEPLOYMENT.md)).

Publishing is automated: pushing a `v*` tag triggers `.github/workflows/release.yml`, which stages
the image with `sbt Docker/stage` and pushes it to ghcr. No manual `docker push` is needed.

## Making a release

1. **Bump the version** to `X.Y.Z`. The release version lives in `build.sbt`
   (`inThisBuild { version := "X.Y.Z" }`) and drives the image tag, `hydrozoa.BuildInfo.version`,
   and `GET /version` — keep it equal to the git tag below. The same image tag is **hard-coded** in
   a few deployment files that do *not* read `build.sbt`, so bump them in lockstep:

   - `src/main/resources/scaffold/hydrozoa.sh` — `HYDROZOA_VERSION` default
   - `docs/user-guide/DEPLOYMENT.md` — the `hydrozoa version` sample output, the `git describe`
     paragraph beneath it, and the `HYDROZOA_VERSION=` / `HYDROZOA_IMAGE=` run examples

   `scaffold/docker-compose.yml` needs **no** edit: its image default is
   `${HYDROZOA_IMAGE:-ghcr.io/cardano-hydrozoa/hydrozoa:${HYDROZOA_VERSION:-latest}}`, so it follows
   `hydrozoa.sh` rather than pinning a version of its own.

   Catch stragglers by grepping the bare previous version, not just the image ref — it also appears
   as `version := "X.Y.Z"` and in prose:

   ```bash
   grep -rn "0\.1\.10" --include=* . | grep -vE "^\./(target|\.git|head)/"
   ```

   Not bumped: `HydrozoaRoutes.apiVersion` + `docs/openapi*.yaml` (the API-contract version,
   separate from the release version). Commit the bump on `main` (via PR; `main` is protected).

2. **Check the baked-in reference-script UTxOs are current.** The image ships per-network default
   reference UTxOs at `src/main/resources/scaffold/ref-utxos/` (Preview and Preprod): the treasury +
   dispute validators and the G2 setup ladder that `build-head-config` falls back to when a head has
   no local `bootstrap/ref-utxos.json` (see
   [DEPLOYMENT.md § Step 4](docs/user-guide/DEPLOYMENT.md)). They sit at the unspendable burn
   address, so they never need redeploying **unless the compiled on-chain scripts changed** since
   they were last deployed.

   - **If this release changes the compiled `cardanoOnchain` scripts** — either the validator/ladder
     source changed, or Scalus was bumped (a compiler bump can alter the compiled UPLC even with no
     source change) — the baked refs are **stale**: a head booting from the released image on those
     networks fails with "invalid treasury/dispute script utxos".

     First, re-export the scripts and rebuild the launcher (once):

     ```bash
     just export    # re-export the blueprint to src/main/resources/hydrozoa/scripts/plutus.json
     just stage     # rebuild the launcher so the deploy below runs the freshly exported scripts
     ```

     Then, on **each** affected network, deploy fresh reference UTxOs from a throwaway single-peer
     head — only head peer 0's wallet funds the deploy, so 1 head / 0 coils / quorum 0 is enough:

     ```bash
     export HYDROZOA_HOME=./head/release/preprod   # or ./head/release/preview
     just scaffold                                 # writes the workspace + config template
     # set blockfrostApiKey in $HYDROZOA_HOME/template/peer-private.template.json.local
     #   (a preprod…/preview… key selects the network)
     just keygen-fleet 1 0 0                        # minimal fleet → head-0's funding wallet
     just head-zero-address                         # prints head-0's L1 address — fund it, then wait
     just deploy-scripts-and-g2-setup               # deploys; prints the new script hashes
     ```

     Then copy the deployed refs from `$HYDROZOA_HOME/bootstrap/ref-utxos.json` into the matching
     `src/main/resources/scaffold/ref-utxos/<network>.json` resource, and confirm the printed script
     hashes match `just export`'s output.
   - **If the scripts are unchanged**, the existing refs stay valid — nothing to do. Smoke-test by
     booting a head on Preprod/Preview from the image (`docker run … serve`); a clean start confirms
     the refs resolve.

3. **Review the shipped logger levels.** The image runs `src/main/resources/logback-docker.xml`
   (console-only, `root` at `warn`), not the verbose local `logback.xml`. Before tagging, confirm
   its levels are what you want to ship.

4. **Sanity-check the image locally** (optional but recommended):

   ```bash
   just docker-image                                   # builds cardano-hydrozoa/hydrozoa:X.Y.Z
   docker run --rm cardano-hydrozoa/hydrozoa:X.Y.Z version
   #   hydrozoa X.Y.Z / git: v… / built: …
   ```

5. **Tag and push** from the merged commit on `main`:

   ```bash
   git checkout main && git pull
   git tag vX.Y.Z            # the leading v is required; must match build.sbt's version
   git push origin vX.Y.Z
   ```

6. **Watch the release workflow** (Actions → Release). It publishes three tags —
   `ghcr.io/cardano-hydrozoa/hydrozoa:X.Y.Z`, `:X.Y`, and `:latest` — each a multi-arch manifest,
   and then appends a **Container image** section to the GitHub release notes with the pull command
   and the digest.

   That last step needs the release to exist. Create it (from the tag, with the generated changelog)
   before or shortly after pushing the tag; if the workflow runs first it logs that it skipped, and
   re-running it links the image. It never appends twice.

7. **Verify** the published image:

   ```bash
   docker pull ghcr.io/cardano-hydrozoa/hydrozoa:X.Y.Z
   docker run --rm ghcr.io/cardano-hydrozoa/hydrozoa:X.Y.Z version
   ```

   The image is self-identifying: `docker inspect` shows the `org.opencontainers.image.version` /
   `.revision` labels, and a running node reports the same via `GET /version`.

## One-time setup

- **Package visibility.** The first push creates the ghcr package as **private**. To let anyone
  pull without authenticating, set it to public once: repo → Packages → `hydrozoa` → Package
  settings → Change visibility → Public. (The workflow itself needs no extra secrets — it
  authenticates with the built-in `GITHUB_TOKEN`.)

## Notes

- **Version ↔ tag must agree.** `build.sbt`'s `version` is baked into the image; the tag names the
  published image. A mismatch means the image reports a different version than its tag.
- **The `git:` line in `hydrozoa version`.** It is raw `git describe --tags --always --dirty
  --abbrev=8`, not the release version — `<nearest-tag>-<commits-since>-g<hash>` (e.g.
  `v0.1.0-3-gabc12345` means 3 commits past `v0.1.0`, at `abc12345`). Building from a tagged commit
  (as the release workflow does) makes it read a clean `vX.Y.Z`; between releases it shows the
  distance from the newest tag. `--tags` is required so lightweight tags (`git tag vX.Y.Z`) resolve
  — without it `git describe` only considers annotated tags and falls back to an older one. The
  release version proper is the `hydrozoa X.Y.Z` line (from `build.sbt`), so this is provenance
  detail, not a mismatch.
- **Architecture.** The image is a multi-arch manifest covering **linux/amd64** and
  **linux/arm64**. No separate build is needed for arm64: the staged context is JVM bytecode, the
  base image (`eclipse-temurin:25-jre`) is multi-arch, and both native dependencies ship aarch64
  objects inside the same jars — `blst-java` as `supranational/blst/Linux/aarch64/libblst.so`,
  `rocksdbjni` as `librocksdbjni-linux-aarch64.so`. The generated Dockerfile's only `RUN` steps are
  `chmod` and `useradd` over 168 files, so QEMU emulation costs seconds rather than a rebuild.
- **No pre-release automation.** Only `v*` tags publish. Ordinary `main` pushes and PRs run checks
  only (`.github/workflows/ci.yml`).
