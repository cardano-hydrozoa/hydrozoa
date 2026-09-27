#!/usr/bin/env python3
"""Decide the repository's required `build` check from the results of the jobs it depends on.

Reads $NEEDS, the workflow's `needs` context as JSON (`${{ toJSON(needs) }}`): for each job, its
`result` (success, failure, cancelled or skipped) and its `outputs`. Passes when the `plan` job
succeeded and set its outputs, and every test job either succeeded or was skipped because the plan
didn't call for it. Anything else fails, with one line per reason: a test job skipped although the
plan called for it would otherwise let a run pass with nothing tested (a mistyped output name reads
as empty, which skips the job).

This is the required check, so it fails closed: input it can't read fails it.
"""

import json
import os
import sys

PLAN = "plan"
# The plan's outputs, which must agree with OUTPUTS in classify-changes.py, the plan job's `outputs:`
# and the test jobs' `if:` conditions in ci.yml.
PLAN_OUTPUTS = ("code", "unit", "integration", "canary")
# Test job -> the plan outputs that call for it. It may be skipped only if all of them are "false".
TEST_JOBS = {
    "tests": ("unit", "integration", "canary"),
    "yaci": ("integration",),
}


def problems(needs):
    plan = needs.get(PLAN, {})
    outputs = plan.get("outputs", {})
    found = []
    if plan.get("result") != "success":
        found.append(f"{PLAN}: {plan.get('result', 'missing')}")
    for name in PLAN_OUTPUTS:
        if outputs.get(name) not in ("true", "false"):
            found.append(f"{PLAN}: output {name} is {outputs.get(name)!r}, not true or false")
    for job, called_by in TEST_JOBS.items():
        result = needs.get(job, {}).get("result", "missing")
        wanted = [o for o in called_by if outputs.get(o) != "false"]
        if result == "success":
            continue
        if result == "skipped" and not wanted:
            continue
        if result == "skipped":
            found.append(f"{job}: skipped, although the plan called for it ({', '.join(wanted)})")
        else:
            found.append(f"{job}: {result}")
    return found


def main():
    needs = json.loads(os.environ["NEEDS"])
    for job, info in needs.items():
        print(f"{job}: {info.get('result')} {json.dumps(info.get('outputs', {}), sort_keys=True)}")
    found = problems(needs)
    if found:
        for p in found:
            print(f"::error title=CI::{p}")
        return 1
    if needs[PLAN]["outputs"]["code"] == "false":
        with open(os.environ.get("GITHUB_STEP_SUMMARY") or os.devnull, "a", encoding="utf-8") as f:
            f.write("Only docs or Markdown changed, so nothing was built or tested.\n")
    print("Passed.")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as e:  # fail closed
        print(f"::error title=CI::can't read the jobs' results ({type(e).__name__}: {e})")
        sys.exit(1)
