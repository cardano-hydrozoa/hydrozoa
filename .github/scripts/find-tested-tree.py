#!/usr/bin/env python3
"""Find whether a passing CI run already tested the tree this run would test.

A run tests a tree, not a commit: the pull_request run tests GitHub's merge of the PR into its base,
the merge queue's run tests the queue's merge commit, and the push to main tests the commit the
queue left there. When main hasn't moved between them, all three have the same tree. A run that
passed and ran the unit and integration tests records its tree as an artifact named
`tested-tree-<tree>` (the `build` job's last step); this script looks for that artifact.

It accepts an artifact only from a successful run of ci.yml in this repository (not a fork), whose
head commit is this commit or this commit's second parent. The second parent is the PR's head in
both the queue's merge commit and main's merge commit, so a PR's own run can vouch for its queue
run, and the queue's run for the push. A marker from any other PR's run can't.

It only answers for push and merge_group runs. Everything else, and any failure, answers false, so
that the run tests everything: a wrong "true" skips tests, a wrong "false" only costs time.

Environment:
  GITHUB_EVENT_NAME   the triggering event (set by the runner)
  GITHUB_REPOSITORY   owner/repo (set by the runner)
  GH_TOKEN            a token that can read the repository's Actions artifacts and runs
  GITHUB_OUTPUT       receives tree=, tested=, tested-by= (optional locally)
"""

import json
import os
import subprocess
import sys

WORKFLOW = ".github/workflows/ci.yml"
EVENTS = ("push", "merge_group")


def git(*args):
    return subprocess.run(["git", *args], check=True, capture_output=True, text=True).stdout.strip()


def gh_api(path):
    out = subprocess.run(["gh", "api", path], check=True, capture_output=True, text=True).stdout
    return json.loads(out)


def candidate_heads():
    """This commit, and its second parent when it is a merge (depth-2 checkout)."""
    heads = {git("rev-parse", "HEAD")}
    second = subprocess.run(["git", "rev-parse", "--verify", "--quiet", "HEAD^2"],
                            capture_output=True, text=True)
    if second.returncode == 0:
        heads.add(second.stdout.strip())
    return heads


def find(tree, repo):
    """The id of a run that vouches for `tree`, or None."""
    heads = candidate_heads()
    listing = gh_api(f"repos/{repo}/actions/artifacts?name=tested-tree-{tree}&per_page=100")
    for artifact in listing.get("artifacts", []):
        run = artifact.get("workflow_run") or {}
        if artifact.get("expired") or run.get("head_sha") not in heads:
            continue
        if run.get("head_repository_id") != run.get("repository_id"):
            continue
        details = gh_api(f"repos/{repo}/actions/runs/{run['id']}")
        if details.get("path", "").split("@")[0] == WORKFLOW and details.get("conclusion") == "success":
            return run["id"]
    return None


def main():
    event = os.environ.get("GITHUB_EVENT_NAME", "")
    tree = git("rev-parse", "HEAD^{tree}")
    tested_by = None
    if event in EVENTS:
        try:
            tested_by = find(tree, os.environ["GITHUB_REPOSITORY"])
        except Exception as e:  # any failure reads as untested
            print(f"::warning title=CI::can't look up earlier runs of tree {tree} "
                  f"({type(e).__name__}: {e}); testing everything")
    if tested_by:
        print(f"Tree {tree} was tested by run {tested_by}.")
    elif event in EVENTS:
        print(f"No passing run of this commit or its PR head recorded tree {tree}.")
    with open(os.environ.get("GITHUB_OUTPUT") or os.devnull, "a") as f:
        f.write(f"tree={tree}\ntested={'true' if tested_by else 'false'}\ntested-by={tested_by or ''}\n")


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as e:  # no tree: nothing is recorded or skipped, and everything is tested
        print(f"::warning title=CI::can't read this commit's tree ({type(e).__name__}: {e})")
        with open(os.environ.get("GITHUB_OUTPUT") or os.devnull, "a") as f:
            f.write("tree=\ntested=false\ntested-by=\n")
