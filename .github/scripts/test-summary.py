#!/usr/bin/env python3
"""Summarise a CI job's test results: sbt's JUnit XML reports plus the test steps' outcomes.

Reads every TEST-*.xml under the given directories (default: all sbt test-report directories
under target/). Writes a Markdown summary to $GITHUB_STEP_SUMMARY (a verdict line, then each
failed test with its suite, its message and the first lines of its trace), and emits one
`::error` annotation per failed test, so a failure is named on the run page without reading the
log. Prints the same summary to stdout when run outside GitHub Actions.

The reports alone can't say that everything ran: sbt writes a suite's report only when the suite
ends, so a compile error, a hang or a crashed test JVM leaves no report, and no report looks like
nothing failed. So the verdict also takes the test steps' outcomes, from $TEST_STEPS, e.g.
"unit=success integration=failure". "Passed" needs every listed step to have succeeded and at least
one report; a step that failed or was cancelled with no failing test in the reports reads "did not
complete". sbt also writes nothing for a cancelled test, so one counts as passed here.

GitHub displays at most 10 error annotations per step, so with more failures the annotations show
the first ten and the summary, which has no limit, lists them all.

Exit status is always 0: the test steps decide pass or fail; this only reports.
"""

import glob
import os
import sys
import xml.etree.ElementTree as ET

TRACE_LINES = 12
COUNTS = ("tests", "failures", "errors", "skipped")
# GitHub drops a step summary over its size limit entirely, and a single ScalaCheck counterexample
# can be that long; so each failure's text is capped, and past this budget, half the limit when
# written, failures are listed by name only.
DETAIL_CHARS = 8_000
SUMMARY_BUDGET = 512 * 1024
ANNOTATION_CHARS = 4_000


def report_files(roots):
    files = []
    for root in roots:
        files += glob.glob(os.path.join(root, "**", "test-reports", "TEST-*.xml"), recursive=True)
    return sorted({os.path.realpath(f) for f in files if os.path.isfile(f)})


def step_outcomes(text):
    """Parses "name=outcome name=outcome"; skipped steps didn't run by design and are left out."""
    steps = []
    for item in (text or "").split():
        name, _, outcome = item.partition("=")
        if outcome != "skipped":
            steps.append((name, outcome or "unknown"))
    return steps


def capped(text, limit):
    return text if len(text) <= limit else text[:limit] + f"\n... ({len(text) - limit} more chars)"


def escape_annotation(text):
    # Workflow-command data must escape %, CR and LF.
    return text.replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A")


def escape_property(text):
    # A property value (title=...) must also escape the `:` and `,` that delimit properties.
    return escape_annotation(text).replace(":", "%3A").replace(",", "%2C")


def main():
    roots = sys.argv[1:] or ["target"]
    files = report_files(roots)
    steps = step_outcomes(os.environ.get("TEST_STEPS"))
    totals = dict.fromkeys(COUNTS, 0)
    failed = []  # (suite, test, kind, message, trace)
    unreadable = []
    for path in files:
        try:
            suite = ET.parse(path).getroot()
        except Exception as e:  # a truncated, empty or undecodable report
            unreadable.append(f"{path}: {e}")
            continue
        # Counted from the test cases, not the suite's attributes, which can disagree with them.
        for case in suite.iter("testcase"):
            totals["tests"] += 1
            for key, tag in (("failures", "failure"), ("errors", "error"), ("skipped", "skipped")):
                if case.find(tag) is not None:
                    totals[key] += 1
            for kind in ("failure", "error"):
                for node in case.findall(kind):
                    trace = (node.text or "").strip().splitlines()
                    failed.append(
                        (
                            suite.get("name", "?"),
                            case.get("name", "?"),
                            kind,
                            (node.get("message") or (trace[0] if trace else "")).strip(),
                            "\n".join(trace[:TRACE_LINES]),
                        )
                    )

    not_ok = [f"{name} {outcome}" for name, outcome in steps if outcome != "success"]
    if failed:
        verdict = "failed"
    elif not_ok:
        verdict = "did not complete"
    elif unreadable:
        verdict = "incomplete, some reports are unreadable"
    elif not files:
        verdict = "not found, no test reports"
    else:
        verdict = "passed"

    out = [
        f"### Tests {verdict}: {totals['tests']} run, {totals['failures']} failed, "
        f"{totals['errors']} errors, {totals['skipped']} skipped ({len(files)} suites)\n"
    ]
    if steps:
        out.append("Steps: " + ", ".join(f"{name} {outcome}" for name, outcome in steps) + ".\n")
    if not_ok and not failed:
        why = (
            f"{', '.join(not_ok)}, and no failing test was reported. A compile error, a hang or a "
            "crashed test JVM leaves no report; see the step's log."
        )
        out.append(why + "\n")
        print(f"::error title=Tests did not complete::{escape_annotation(why)}")
    if unreadable:
        out.append("Unreadable reports:\n")
        out += [f"- `{u}`" for u in unreadable]
        out.append("")
    size = sum(len(line.encode()) + 1 for line in out)
    names_only = []
    for suite, test, kind, message, trace in failed:
        print(
            f"::error title={escape_property(test[:200])}::"
            f"{escape_annotation(capped(message or kind, ANNOTATION_CHARS))}"
        )
        if size > SUMMARY_BUDGET:
            names_only.append(f"- {kind}: `{test}` in `{suite}`")
            continue
        detail = capped(message if message else "(no message)", DETAIL_CHARS)
        if trace and trace != message:
            detail += "\n\n" + capped(trace, DETAIL_CHARS)
        block = [f"#### {kind}: `{test}`\n", f"Suite `{suite}`\n", "```text", detail, "```\n"]
        out += block
        size += sum(len(line.encode()) + 1 for line in block)
    if names_only:
        out.append(f"{len(names_only)} more failures, by name only (the summary's size budget):\n")
        out += names_only

    text = "\n".join(out) + "\n"
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        with open(summary, "a", encoding="utf-8") as f:
            f.write(text)
    else:
        sys.stdout.write(text)


if __name__ == "__main__":
    try:
        sys.stdout.reconfigure(errors="replace")
        main()
    except BaseException as e:  # never fail the job; say that the summary is missing, and why
        why = f"the summary failed ({type(e).__name__}: {e}); see the test steps' logs"
        print(f"::warning title=Test summary::{escape_annotation(why)}")
    sys.exit(0)
