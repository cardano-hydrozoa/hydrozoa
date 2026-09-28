#!/usr/bin/env python3
"""Decide which CI tests run (the `unit`, `integration` and `yaci` jobs), from the files a pull
request changes.

Every changed path falls into one bucket; the first matching rule in BUCKETS wins:
  unit         under docs/api/: the unit tests check these schemas against the code
               (OpenApiSchemaTest), so they are test inputs, not documentation
  docs         under docs/, or any *.md file anywhere
  reporting    what CI's test reporting depends on: the build (build.sbt, project/), Nix, the
               justfile, .github/, the test-event recorder (src/test/scala/test/) and the reporting
               canary (ci-canary/)
  core-test    under src/test/: the unit tests, and also a dependency of the integration suites,
               which build on core's test code (`core % "test->test"` in build.sbt)
  integration  under integration/
  other        anything else, such as production code and the root dotfiles
A rename counts both its old and its new path; a deletion counts its path. What each bucket runs is
in RUNS: `other` and `core-test` run all the tests, and `reporting` also the reporting canary
(`just ci-canary`); docs alone, or no files at all, run nothing heavy. Any event but pull_request
(merge_group, workflow_dispatch, ...) runs everything, the canary included, except a push or
merge_group run whose tree TREE_TESTED marks as already tested by a passing run: that runs no tests.

The change set of a pull_request run is HEAD^1..HEAD. For that event actions/checkout checks out
GitHub's test merge (refs/pull/N/merge): a two-parent commit whose first parent is the base branch
and whose second is the PR head. Its difference from the first parent is exactly how the tree under
test differs from the base, needs no merge-base search, and has no file-count limit (the REST API
lists at most 3000 files per PR). Checkout with `fetch-depth: 2` makes both parents local; with
less, the first parent is fetched by its SHA.

It fails safe: if the change set can't be established, or anything unexpected happens, everything
runs, and the step summary and a warning annotation say why. Tests are never skipped because
something went wrong. Only a failure to write the outputs themselves fails the step.

Environment:
  GITHUB_EVENT_NAME    the triggering event (set by the runner)
  PR_HEAD_SHA          ${{ github.event.pull_request.head.sha }}; pull_request only
  PR_BASE_SHA          ${{ github.event.pull_request.base.sha }}; optional, only reported
  TREE_TESTED          "true" when a passing run already tested this tree; push and merge_group only
  TESTED_BY            the id of that run, for the report
  GITHUB_OUTPUT        receives code=, unit=, integration=, canary= as true/false (optional locally)
  GITHUB_STEP_SUMMARY  receives a readable summary (optional locally)

To try it locally, run it in a clone whose HEAD is a merge of a branch into its base, e.g.
  git switch --detach main && git merge --no-ff --no-edit <branch>
  GITHUB_EVENT_NAME=pull_request PR_HEAD_SHA=$(git rev-parse <branch>) \\
    .github/scripts/classify-changes.py
"""

import os
import subprocess
import sys

# (bucket, test): the first rule whose test matches a path decides its bucket.
BUCKETS = [
    ("unit", lambda p: p.startswith("docs/api/")),
    ("docs", lambda p: p.startswith("docs/") or p.endswith(".md")),
    (
        "reporting",
        lambda p: p in ("build.sbt", "flake.nix", "flake.lock", "justfile")
        or p.startswith(("project/", ".github/", "src/test/scala/test/", "ci-canary/")),
    ),
    ("core-test", lambda p: p.startswith("src/test/")),
    ("integration", lambda p: p.startswith("integration/")),
    ("other", lambda p: True),
]
# What a bucket runs. Docs run nothing heavy.
RUNS = {
    "unit": {"unit"},
    "docs": set(),
    "reporting": {"unit", "integration", "canary"},
    "core-test": {"unit", "integration"},
    "integration": {"integration"},
    "other": {"unit", "integration"},
}
OUTPUTS = ("unit", "integration", "canary")
# Above this many files the step summary shows the counts only; the log always lists them all.
SUMMARY_FILE_LIMIT = 200


class RunEverything(Exception):
    """The change set can't be established; the message says why."""


def bucket_of(path):
    return next(name for name, matches in BUCKETS if matches(path))


def shown(path):
    """Renders a path for one line of output. A name holding control characters (a newline, say) is
    shown $'...'-quoted, so it can never start a line of its own in the log, where the runner would
    read a leading `::` as a workflow command."""
    if any(ord(c) < 32 or ord(c) == 127 or 0xDC80 <= ord(c) <= 0xDCFF for c in path):
        body = path.encode("utf-8", "surrogateescape").decode("latin-1")
        return "$'" + body.encode("unicode_escape").decode("ascii").replace("'", "\\'") + "'"
    return path


def git(*args, check=True):
    r = subprocess.run(["git", *args], capture_output=True)
    if check and r.returncode != 0:
        err = r.stderr.decode(errors="replace").strip()
        raise RunEverything(f"`git {' '.join(args)}` failed: {err}")
    return r


def pr_changes():
    """The (status, path) pairs of the pull request's test merge commit against its first parent."""
    head_sha = os.environ.get("PR_HEAD_SHA")
    if not head_sha:
        raise RunEverything("PR_HEAD_SHA is not set, so the PR head can't be checked")
    r = git("rev-parse", "--verify", "--quiet", "HEAD^{commit}", check=False)
    if r.returncode != 0:
        raise RunEverything("no commit is checked out")
    head = r.stdout.decode().strip()
    # The parents come from the raw commit object: in a shallow clone the checked-out commit's
    # parents are cut off, and `HEAD^1` would not resolve even though the object names them.
    header = git("cat-file", "commit", head).stdout.decode(errors="replace").split("\n\n", 1)[0]
    parents = [line[len("parent "):] for line in header.splitlines() if line.startswith("parent ")]
    if len(parents) != 2:
        n = len(parents)
        raise RunEverything(f"HEAD {head} has {n} parent(s), not the 2 of a PR test merge")
    base, pr_head = parents
    if pr_head != head_sha:
        raise RunEverything(f"HEAD's second parent {pr_head} is not the PR head {head_sha}")
    event_base = os.environ.get("PR_BASE_SHA")
    if event_base and event_base != base:
        print(f"Note: the event's base {event_base} differs from the merge's first parent {base};"
              " the base branch moved, and the merge is what is tested.")
    if git("cat-file", "-e", f"{base}^{{commit}}", check=False).returncode != 0:
        print(f"Base {base} is not in the clone; fetching it.")
        fetch = ["fetch", "--quiet", "--no-tags", "--no-recurse-submodules", "--depth=1"]
        if git(*fetch, "origin", base, check=False).returncode != 0:
            raise RunEverything(f"the base commit {base} is missing and could not be fetched")
    # Plumbing, so no user or repository diff setting applies; --no-renames reports a rename as a
    # deletion plus an addition, so both paths count; -z passes every name through byte for byte.
    raw = git("diff-tree", "-r", "-z", "--no-renames", "--name-status", base, head).stdout
    fields = raw.split(b"\0")
    if fields and fields[-1] == b"":
        fields.pop()
    if len(fields) % 2:
        raise RunEverything("unexpected git diff-tree output (a status without a path)")
    pairs = [
        (fields[i].decode(), fields[i + 1].decode("utf-8", "surrogateescape"))
        for i in range(0, len(fields), 2)
    ]
    return f"{base[:12]}..{head[:12]}", pairs


def plan_text(runs):
    if not runs:
        return "nothing heavy: the change is docs or Markdown only, or empty"
    parts = ["precommit checks"]
    if "unit" in runs:
        parts.append("unit tests")
    if "integration" in runs:
        parts.append("integration tests (Yaci included)")
    if "canary" in runs:
        parts.append("the reporting canary")
    return ", ".join(parts)


def main():
    event = os.environ.get("GITHUB_EVENT_NAME", "")
    forced = ""  # why everything runs regardless of the files; empty means the files decide
    change_set = ""
    changes = []
    if event == "pull_request":
        try:
            change_set, changes = pr_changes()
        except RunEverything as e:
            forced = str(e)
    elif not event:
        forced = "GITHUB_EVENT_NAME is not set"
    elif event in ("push", "merge_group") and os.environ.get("TREE_TESTED") == "true":
        # No tests, but code: a push's unit job still compiles, to save the compile cache.
        outputs = {"code": True, **{name: False for name in OUTPUTS}}
        why = f"run {os.environ.get('TESTED_BY') or '?'} already tested this tree"
        report(event, "", "", {}, [], set(), outputs, warn=False, skipped=why)
        return
    else:
        forced = f"the '{event}' event always runs everything"

    counts = {name: 0 for name, _ in BUCKETS}
    listing = []
    runs = set()
    for status, path in changes:
        bucket = bucket_of(path)
        counts[bucket] += 1
        runs |= RUNS[bucket]
        listing.append(f"{bucket:<12} {status} {shown(path)}")
    if forced:
        runs = set(OUTPUTS)
    outputs = {"code": bool(runs), **{name: name in runs for name in OUTPUTS}}
    # An anticipated failure on a pull_request is worth an annotation; other events are by design.
    warn = event == "pull_request" and bool(forced)
    report(event, forced, change_set, counts, listing, runs, outputs, warn=warn)


def report(event, forced, change_set, counts, listing, runs, outputs, warn, skipped=""):
    flags = " ".join(f"{k}={str(v).lower()}" for k, v in outputs.items())
    with open(os.environ.get("GITHUB_OUTPUT") or os.devnull, "a") as f:
        f.write("".join(f"{k}={str(v).lower()}\n" for k, v in outputs.items()))

    plan = f"no tests: {skipped}" if skipped else plan_text(runs)
    count_text = ", ".join(f"{name} {n}" for name, n in counts.items())
    if warn:
        print(f"::warning title=CI plan::Running everything: {forced}.")
    print(f"Event: {event or '<unset>'}")
    if change_set:
        head = os.environ["PR_HEAD_SHA"][:12]
        print(f"Change set: {change_set}, the PR head {head} merged into its base")
        print(f"Files: {len(listing)} ({count_text})")
        print("::group::Changed files by bucket")
        for line in listing:
            print(line)
        print("::endgroup::")
    if forced:
        print(f"Everything runs: {forced}.")
    print(f"Plan: {plan}")
    print(flags)

    summary = ["### CI plan", "", f"**Runs:** {plan}.", ""]
    if forced:
        summary += [f"Everything runs because {forced}.", ""]
    if change_set:
        summary += [
            f"Change set `{change_set}` (the PR head merged into its base): "
            f"{len(listing)} files ({count_text}).",
            "",
        ]
        if 0 < len(listing) <= SUMMARY_FILE_LIMIT:
            summary += ["```text", *listing, "```"]
        elif listing:
            summary.append(f"Over {SUMMARY_FILE_LIMIT} files: the step log lists them all.")
    with open(os.environ.get("GITHUB_STEP_SUMMARY") or os.devnull, "a", encoding="utf-8") as f:
        f.write("\n".join(summary) + "\n")


if __name__ == "__main__":
    sys.stdout.reconfigure(errors="backslashreplace")
    try:
        main()
    except Exception as e:  # anything unexpected: run everything, and say why
        why = f"the classifier failed ({type(e).__name__}: {e})".replace("\n", " ")
        everything = {"code": True, **{name: True for name in OUTPUTS}}
        report(os.environ.get("GITHUB_EVENT_NAME", ""), why, "", {}, [], set(OUTPUTS), everything,
               warn=True)
