# Source this to get the `hydrozoa` Docker CLI alias, so you don't retype the docker run flags:
#
#     source ./hydrozoa.sh
#     hydrozoa keygen-fleet 2 4 2
#     hydrozoa build-head-config
#
# The Blockfrost key comes from head/template/peer-private.template.json.local (`blockfrostApiKey`);
# `export BLOCKFROST_API_KEY=preview…` before sourcing overrides it for the chain-querying commands.
#
# Override the version or head directory before sourcing (or edit the defaults below). HYDROZOA_HOME
# is the one head directory the CLI and `docker compose` (DEPLOYMENT.md §4) both read.
: "${HYDROZOA_VERSION:=0.1.1}"      # published image tag (ghcr.io/cardano-hydrozoa/hydrozoa)
: "${HYDROZOA_HOME:=./head/demo}"  # head directory
export HYDROZOA_VERSION HYDROZOA_HOME

# Runs the image's `hydrozoa` CLI: passes the Blockfrost key + HYDROZOA_HOME through, enables host
# networking + a TTY (for the interactive submit-* commands), and runs as container-root so it can
# write the mounted head/ dir (see the rootless-Docker note in DEPLOYMENT.md §2).
alias hydrozoa='docker run --rm -it --network host -e BLOCKFROST_API_KEY -e HYDROZOA_HOME \
  --user root -v "$PWD/head:/work/head" -w /work \
  ghcr.io/cardano-hydrozoa/hydrozoa:"$HYDROZOA_VERSION"'
