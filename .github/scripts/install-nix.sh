#!/usr/bin/env bash
# Install Nix on a GitHub-hosted Ubuntu runner, with Nix's own installer, pinned and verified.
#
# The install script for a Nix release records the SHA-256 of each platform's Nix tarball and
# checks it, so pinning the script's hash pins the binaries too. The hash below is NixOS's
# published one (https://releases.nixos.org/nix/nix-<version>/install.sha256). To upgrade, change
# both values together.
#
# Multi-user (daemon) mode: builds run as dedicated build users under a root daemon, so the build
# sandbox works on Ubuntu 24.04, which restricts the unprivileged user namespaces that single-user
# mode's sandbox needs. The runner has systemd and passwordless sudo.
#
# Environment:
#   GITHUB_TOKEN  optional; the job's token, given to Nix for github.com so that fetching the flake's
#                 github: inputs is not subject to GitHub's anonymous rate limit (runners share IPs)
#   GITHUB_PATH   set by the runner; Nix's bin directory is added to it for the later steps
set -euo pipefail

readonly NIX_VERSION=2.35.2
readonly INSTALL_SHA256=9adda97297d9e8ab360df95c729eabff4f4f93d6db091953c3a68f29e3fb130c

work=$(mktemp -d "${RUNNER_TEMP:-${TMPDIR:-/tmp}}/install-nix.XXXXXX")
trap 'rm -rf -- "${work}"' EXIT

curl --fail --silent --show-error --location --retry 5 --retry-all-errors \
  --output "${work}/install" "https://releases.nixos.org/nix/nix-${NIX_VERSION}/install"
echo "${INSTALL_SHA256}  ${work}/install" | sha256sum --check --strict -

{
  echo "experimental-features = nix-command flakes"
  echo "max-jobs = auto"
  # printf is a builtin, so the token is never in a process's argument list; ${work} is private
  # (mode 0700) and removed on exit, and GitHub masks the token's value in the log.
  if [[ -n ${GITHUB_TOKEN:-} ]]; then printf 'access-tokens = github.com=%s\n' "${GITHUB_TOKEN}"; fi
} >"${work}/nix.conf"

sh "${work}/install" --daemon --yes --no-channel-add --nix-extra-conf-file "${work}/nix.conf"

echo "/nix/var/nix/profiles/default/bin" >>"${GITHUB_PATH:-/dev/null}"
# Proves the daemon answers, so a broken install fails this step rather than the first `nix develop`.
/nix/var/nix/profiles/default/bin/nix store info
