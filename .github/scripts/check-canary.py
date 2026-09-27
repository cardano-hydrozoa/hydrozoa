#!/usr/bin/env python3
"""Check the reporting canary: CI's summary must report each of the canary's known outcomes exactly.

`just ci-canary` runs the ci-canary project in three stages and keeps each stage's result files
under <dir>/<stage>/ci-events, and sbt's exit status in <dir>/<stage>/sbt-exit-status. This runs
test-summary.py on each stage, as CI runs it on a job, and compares its --json result with what
the canary's sources make certain (ci-canary/src/test/scala/canary/, ci-canary/broken/). Any
difference fails, with one line per difference: it means the reporting would misread a real run.

Usage: check-canary.py <dir>, from the repository root.
"""

import json
import os
import subprocess
import sys
import tempfile

SUMMARY = os.path.join(os.path.dirname(os.path.abspath(__file__)), "test-summary.py")

# stage -> the result fields the summary must give, exactly.
EXPECTED = {
    # CanarySuite: passes, fails, is cancelled, is ignored, is pending. CanaryProperties: holds, is
    # falsified. CanaryHalt: passes (it halts only when asked).
    "outcomes": {
        "verdict": "failed",
        "states": ["failed"],
        "passed": 3,
        "failed": 2,
        "errors": 0,
        "cancelled": 1,
        "ignored": 1,
        "skipped": 0,
        "pending": 1,
        "unknown": 0,
        "failures": [
            {
                "suite": "canary.CanarySuite",
                "test": "fails, by design: with a comma and a colon",
                "status": "Failure",
            },
            {
                "suite": "canary.CanaryProperties",
                "test": "CanaryProperties.is falsified, by design",
                "status": "Failure",
            },
        ],
        "cancelledTests": [{"suite": "canary.CanarySuite", "test": "is cancelled"}],
        "unfinishedSuites": [],
        "compileErrors": [],
        "invalidFiles": [],
        "versionErrors": [],
    },
    # CanaryHalt halts its JVM mid-suite.
    "halt": {
        "verdict": "did-not-finish",
        "states": ["did-not-finish"],
        "failed": 0,
        "unfinishedSuites": ["canary.CanaryHalt"],
        "unfinishedRunners": 1,
        "invalidFiles": [],
        "versionErrors": [],
    },
    # ci-canary/broken/CanaryTypeError.scala: `val n: Int = "not an Int"`, line 5, column 18.
    "compile-error": {
        "verdict": "did-not-complete",
        "states": ["did-not-complete", "compile-failed", "no-test-events"],
        "compileErrors": [
            {"file": "ci-canary/broken/CanaryTypeError.scala", "line": 5, "column": 18, "code": "7"}
        ],
        "invalidFiles": [],
        "versionErrors": [],
    },
}


def check(stage, stage_dir, expected):
    problems = []
    try:
        status = int(open(os.path.join(stage_dir, "sbt-exit-status")).read().strip())
    except (OSError, ValueError) as e:
        return [f"{stage}: no sbt exit status ({e})"]
    if status == 0:
        problems.append(f"{stage}: sbt exited 0, but every stage fails by design")
    with tempfile.NamedTemporaryFile(suffix=".json") as out:
        env = dict(os.environ, TEST_STEPS="canary=failure")
        env.pop("GITHUB_STEP_SUMMARY", None)
        run = subprocess.run(
            [sys.executable, SUMMARY, "--json", out.name, stage_dir],
            env=env,
            capture_output=True,
            text=True,
        )
        try:
            got = json.load(open(out.name))
        except (OSError, ValueError) as e:
            return problems + [f"{stage}: no result from the summary ({e}): {run.stdout[-500:]}"]
    for key, want in expected.items():
        have = got.get(key)
        if key == "compileErrors" and isinstance(have, list):
            have = [{k: e.get(k) for k in ("file", "line", "column", "code")} for e in have]
        if key == "failures" and isinstance(have, list):
            have = [{k: f.get(k) for k in ("suite", "test", "status")} for f in have]
            have, want = sorted(have, key=str), sorted(want, key=str)
        if have != want:
            problems.append(f"{stage}: {key} is {have!r}, expected {want!r}")
    return problems


def main():
    root = sys.argv[1]
    problems = []
    for stage, expected in EXPECTED.items():
        problems += check(stage, os.path.join(root, stage), expected)
    for p in problems:
        print(f"::error title=Reporting canary::{p}".replace("\n", " "))
    if problems:
        print(f"The reporting canary failed: {len(problems)} differences.")
        return 1
    print(f"The reporting canary passed: {len(EXPECTED)} stages reported exactly as expected.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
