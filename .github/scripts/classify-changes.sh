#!/usr/bin/env bash
# Decide what the CI `build` job runs, from the files a pull request changes.
#
# Every changed path falls into one bucket, checked in this order:
#   docs         under docs/, or any *.md file anywhere
#   unit         under src/test/
#   integration  under integration/
#   other        anything else: production code, build, Nix, workflows, scripts, dotfiles
# A rename counts both its old and its new path; a deletion counts its path.
#
# The plan: any `other` file runs everything; otherwise `unit` files run the precommit checks and
# the unit tests, `integration` files the precommit checks and the integration suites (Yaci
# included), and docs alone, or no files at all, run nothing heavy. Any event but pull_request
# (merge_group, workflow_dispatch, ...) runs everything.
#
# The change set of a pull_request run is HEAD^1..HEAD. For that event actions/checkout checks
# out GitHub's test merge (refs/pull/N/merge): a two-parent commit whose first parent is the base
# branch and whose second is the PR head. Its difference from the first parent is exactly how the
# tree under test differs from the base, needs no merge-base search, and has no file-count limit
# (the REST API lists at most 3000 files per PR). Checkout with `fetch-depth: 2` makes both parents
# local; with less, the first parent is fetched by its SHA.
#
# It fails safe: if the change set can't be established, everything runs, and the step summary
# and a warning annotation say why. Tests are never skipped because something went wrong.
#
# Environment:
#   GITHUB_EVENT_NAME    the triggering event (set by the runner)
#   PR_HEAD_SHA          ${{ github.event.pull_request.head.sha }}; pull_request only
#   PR_BASE_SHA          ${{ github.event.pull_request.base.sha }}; optional, only reported
#   GITHUB_OUTPUT        receives code=, unit=, integration= as true/false (optional locally)
#   GITHUB_STEP_SUMMARY  receives a readable summary (optional locally)
#
# To try it locally, run it in a clone whose HEAD is a merge of a branch into its base, e.g.
#   git switch --detach main && git merge --no-ff --no-edit <branch>
#   GITHUB_EVENT_NAME=pull_request PR_HEAD_SHA=$(git rev-parse <branch>) \
#     .github/scripts/classify-changes.sh
set -euo pipefail
shopt -s inherit_errexit
trap 'echo "::error title=classify-changes::unexpected failure at line ${LINENO}" >&2' ERR

# Above this many files the step summary shows the counts only; the log always lists them all.
readonly SUMMARY_FILE_LIMIT=200

event=${GITHUB_EVENT_NAME:-}
output=${GITHUB_OUTPUT:-/dev/null}
summary=${GITHUB_STEP_SUMMARY:-/dev/null}

# Why everything runs regardless of the files, if it does. Empty means the files decide.
forced=""
# A human-readable description of the change set that was classified.
range=""
n_docs=0 n_unit=0 n_integration=0 n_other=0
listing=""
listed=0
tmp=""
trap '[[ -z ${tmp} ]] || rm -f -- "${tmp}"' EXIT

# Sets `bucket` for the path in $1. Patterns are anchored at the repository root, and `*` in a
# `case` pattern also matches `/`.
classify() {
  case $1 in
    docs/* | *.md) bucket=docs ;;
    src/test/*) bucket=unit ;;
    integration/*) bucket=integration ;;
    *) bucket=other ;;
  esac
}

# Renders a path for one line of output: names holding control characters (a newline, say) are shown
# $'...'-quoted, so a file name can never start a line of its own in the log, where the runner
# would read a leading `::` as a workflow command.
# Sets `name`.
shown() {
  if [[ $1 == *[[:cntrl:]]* ]]; then
    printf -v name '%q' "$1"
  else
    name=$1
  fi
}

# Fills the counts and `listing` from the pull request's test merge commit. On failure, returns 1
# with `forced` saying why.
collect_pr_changes() {
  local head raw line status path name
  local -a parents=()

  if [[ -z ${PR_HEAD_SHA:-} ]]; then
    forced="PR_HEAD_SHA is not set, so the PR head can't be checked"
    return 1
  fi
  if ! head=$(git rev-parse --verify --quiet 'HEAD^{commit}'); then
    forced="no commit is checked out"
    return 1
  fi
  # The parents come from the raw commit object: in a shallow clone the checked-out commit's
  # parents are cut off, and `HEAD^1` would not resolve even though the object names them.
  if ! raw=$(git cat-file commit "${head}"); then
    forced="can't read commit ${head}"
    return 1
  fi
  while IFS= read -r line; do
    [[ -n ${line} ]] || break
    if [[ ${line} == "parent "* ]]; then parents+=("${line#parent }"); fi
  done <<<"${raw}"

  if ((${#parents[@]} != 2)); then
    forced="HEAD ${head} has ${#parents[@]} parent(s), not the 2 of a PR test merge"
    return 1
  fi
  if [[ ${parents[1]} != "${PR_HEAD_SHA}" ]]; then
    forced="HEAD's second parent ${parents[1]} is not the PR head ${PR_HEAD_SHA}"
    return 1
  fi
  if [[ -n ${PR_BASE_SHA:-} && ${parents[0]} != "${PR_BASE_SHA}" ]]; then
    echo "Note: the event's base ${PR_BASE_SHA} differs from the merge's first parent" \
      "${parents[0]}; the base branch moved, and the merge is what is tested."
  fi

  if ! git cat-file -e "${parents[0]}^{commit}" 2>/dev/null; then
    echo "Base ${parents[0]} is not in the clone; fetching it."
    if ! git fetch --quiet --no-tags --no-recurse-submodules --depth=1 origin "${parents[0]}"; then
      forced="the base commit ${parents[0]} is missing and could not be fetched"
      return 1
    fi
  fi

  tmp=$(mktemp "${RUNNER_TEMP:-${TMPDIR:-/tmp}}/changes.XXXXXX")
  # Plumbing, so no user or repository diff setting applies; --no-renames reports a rename as a
  # deletion plus an addition, so both paths count; -z passes every name through byte for byte.
  if ! git diff-tree -r -z --no-renames --name-status "${parents[0]}" "${head}" >"${tmp}"; then
    forced="git diff-tree ${parents[0]} ${head} failed"
    return 1
  fi
  range="${parents[0]:0:12}..${head:0:12}"

  while :; do
    status="" path=""
    IFS= read -r -d '' status || break
    if ! IFS= read -r -d '' path; then
      forced="unexpected git diff-tree output (a status without a path)"
      return 1
    fi
    classify "${path}"
    case ${bucket} in
      docs) n_docs=$((n_docs + 1)) ;;
      unit) n_unit=$((n_unit + 1)) ;;
      integration) n_integration=$((n_integration + 1)) ;;
      *) n_other=$((n_other + 1)) ;;
    esac
    shown "${path}"
    printf -v line '%-12s %s %s\n' "${bucket}" "${status}" "${name}"
    listing+=${line}
    listed=$((listed + 1))
  done <"${tmp}"
}

if [[ ${event} == pull_request ]]; then
  # Called in a condition, so errexit is off inside it: it checks each step itself and, on a
  # failure, leaves `forced` set, which runs everything.
  collect_pr_changes || true
elif [[ -z ${event} ]]; then
  forced="GITHUB_EVENT_NAME is not set"
else
  forced="the '${event}' event always runs everything"
fi

unit=false integration=false
if [[ -n ${forced} ]] || ((n_other > 0)); then
  unit=true integration=true
else
  if ((n_unit > 0)); then unit=true; fi
  if ((n_integration > 0)); then integration=true; fi
fi
code=false
if [[ ${unit} == true || ${integration} == true ]]; then code=true; fi

if [[ ${unit} == true && ${integration} == true ]]; then
  plan="precommit checks, unit tests, integration tests (Yaci included)"
elif [[ ${unit} == true ]]; then
  plan="precommit checks, unit tests"
elif [[ ${integration} == true ]]; then
  plan="precommit checks, integration tests (Yaci included)"
else
  plan="nothing heavy: the change is docs or Markdown only, or empty"
fi

{
  echo "code=${code}"
  echo "unit=${unit}"
  echo "integration=${integration}"
} >>"${output}"

counts="docs ${n_docs}, unit ${n_unit}, integration ${n_integration}, other ${n_other}"
# An anticipated failure on a pull_request is worth an annotation; the other events are by design.
if [[ ${event} == pull_request && -n ${forced} ]]; then
  echo "::warning title=CI plan::Running everything: ${forced}."
fi
echo "Event: ${event:-<unset>}"
if [[ -n ${range} ]]; then
  echo "Change set: ${range}, the PR head ${PR_HEAD_SHA:0:12} merged into its base"
  echo "Files: ${listed} (${counts})"
  echo "::group::Changed files by bucket"
  printf '%s' "${listing}"
  echo "::endgroup::"
fi
if [[ -n ${forced} ]]; then echo "Everything runs: ${forced}."; fi
echo "Plan: ${plan}"
echo "code=${code} unit=${unit} integration=${integration}"

{
  echo "### CI plan"
  echo
  echo "**Runs:** ${plan}."
  echo
  if [[ -n ${forced} ]]; then echo "Everything runs because ${forced}."; echo; fi
  if [[ -n ${range} ]]; then
    echo "Change set \`${range}\` (the PR head merged into its base): ${listed} files (${counts})."
    echo
    if ((listed == 0)); then
      :
    elif ((listed <= SUMMARY_FILE_LIMIT)); then
      echo '```text'
      printf '%s' "${listing}"
      echo '```'
    else
      echo "Over ${SUMMARY_FILE_LIMIT} files: the step log lists them all."
    fi
  fi
} >>"${summary}"
