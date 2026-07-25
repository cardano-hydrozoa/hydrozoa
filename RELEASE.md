# Releasing Hydrozoa

A release publishes the `hydrozoa` Docker image to the GitHub Container Registry
(`ghcr.io/cardano-hydrozoa/hydrozoa`). The image bundles the whole CLI — the head-node server
(`serve`) and every deployment command — so it is the single artifact a deployer needs (see
[DEPLOYMENT.md](DEPLOYMENT.md)).

Publishing is automated: pushing a `v*` tag triggers `.github/workflows/release.yml`, which stages
the image with `sbt Docker/stage` and pushes it to ghcr. No manual `docker push` is needed.

## Making a release

1. **Pick the version** and set it in `build.sbt` (`inThisBuild { version := "X.Y.Z" }`). This
   drives the image tag, `hydrozoa.BuildInfo.version`, and `GET /version` — keep it equal to the
   git tag below. Commit the bump on `main` (via PR; `main` is protected).

2. **Sanity-check the image locally** (optional but recommended):

   ```bash
   just docker-image                                   # builds cardano-hydrozoa/hydrozoa:X.Y.Z
   docker run --rm cardano-hydrozoa/hydrozoa:X.Y.Z version
   #   hydrozoa X.Y.Z / git: v… / built: …
   ```

3. **Tag and push** from the merged commit on `main`:

   ```bash
   git checkout main && git pull
   git tag vX.Y.Z            # the leading v is required; must match build.sbt's version
   git push origin vX.Y.Z
   ```

4. **Watch the release workflow** (Actions → Release). It publishes three tags:
   `ghcr.io/cardano-hydrozoa/hydrozoa:X.Y.Z`, `:X.Y`, and `:latest`.

5. **Verify** the published image:

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
- **Architecture.** The image is currently linux/amd64 only. Multi-arch (adding linux/arm64 via
  buildx) is a future improvement — it needs the native deps (blst-java, rocksdbjni) verified on
  arm64 first.
- **No pre-release automation.** Only `v*` tags publish. Ordinary `main` pushes and PRs run checks
  only (`.github/workflows/ci.yml`).
