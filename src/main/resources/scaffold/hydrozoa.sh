# Source this to get the `hydrozoa` Docker CLI alias, so you don't retype the docker run flags:
#
#     source ./hydrozoa.sh
#     hydrozoa keygen-fleet 2 4 2
#     hydrozoa build-head-config
#
# The Blockfrost key comes from $HYDROZOA_HOME/template/peer-private.template.json.local
# (`blockfrostApiKey`); `export BLOCKFROST_API_KEY=preview…` before sourcing overrides it for the
# chain-querying commands.
#
# HYDROZOA_HOME is the one head directory the CLI and `docker compose`
# (docs/user-guide/DEPLOYMENT.md §4) both read. It defaults to this file's own directory — the
# scaffolded workspace — resolved to an absolute path, so the alias mounts correctly no matter which
# directory you run it from. Override HYDROZOA_HOME (to an absolute path) or HYDROZOA_VERSION before
# sourcing, or edit the defaults below.
: "${HYDROZOA_VERSION:=0.1.11}"   # published image tag (ghcr.io/cardano-hydrozoa/hydrozoa)
: "${HYDROZOA_HOME:=$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)}"   # head directory (absolute)
export HYDROZOA_VERSION HYDROZOA_HOME

# Runs the image's `hydrozoa` CLI: mounts HYDROZOA_HOME as the in-container workdir /work — every
# file the CLI reads or writes (template, bootstrap, private configs, head-config) lives under that
# one head directory, so nothing else needs mounting. Passes the Blockfrost key through, enables host
# networking + a TTY (for the interactive submit-* commands), and runs as container-root so it can
# write the mounted dir (see the rootless-Docker note in docs/user-guide/DEPLOYMENT.md §2).
alias hydrozoa='docker run --rm -it --network host -e BLOCKFROST_API_KEY -e HYDROZOA_HOME=/work \
  --user root -v "$HYDROZOA_HOME:/work" -w /work \
  ghcr.io/cardano-hydrozoa/hydrozoa:"$HYDROZOA_VERSION"'
