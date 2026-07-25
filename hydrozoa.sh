# Source this to get the `hydrozoa` Docker CLI alias, so you don't retype the docker run flags:
#
#     export BLOCKFROST_API_KEY=preview…      # your key (a secret — not stored here)
#     source ./hydrozoa.sh
#     hydrozoa keygen-fleet 2 4 2
#     hydrozoa build-head-config
#
# Override the version or config dir before sourcing (or edit the defaults below). HYDROZOA_HOME is
# the one config directory the CLI and `docker compose` (DEPLOYMENT.md §4) both read.
: "${HYDROZOA_VERSION:=0.1.0}"       # published image tag (ghcr.io/cardano-hydrozoa/hydrozoa)
: "${HYDROZOA_HOME:=./config/demo}"  # config directory
export HYDROZOA_VERSION HYDROZOA_HOME

# Runs the image's `hydrozoa` CLI: passes the Blockfrost key + HYDROZOA_HOME through, enables host
# networking + a TTY (for the interactive submit-* commands), and runs as container-root so it can
# write the mounted config dir (see the rootless-Docker note in DEPLOYMENT.md §2).
alias hydrozoa='docker run --rm -it --network host -e BLOCKFROST_API_KEY -e HYDROZOA_HOME \
  --user root -v "$PWD/config:/work/config" -w /work \
  ghcr.io/cardano-hydrozoa/hydrozoa:"$HYDROZOA_VERSION"'
