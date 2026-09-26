#!/usr/bin/env python3
"""Summarise sbt's JUnit XML test reports for a CI run.

Reads every TEST-*.xml under the given directories (default: all sbt test-report directories
under target/). Writes a Markdown summary to $GITHUB_STEP_SUMMARY (a table of totals per suite
with failures, then each failed test with its message and the first lines of its trace), and
emits one `::error` annotation per failed test, so a failure is named on the run page without
reading the log. Prints the same summary to stdout when run outside GitHub Actions.

Exit status is always 0: the test steps decide pass or fail; this only reports.
"""

import glob
import os
import sys
import xml.etree.ElementTree as ET

TRACE_LINES = 12


def report_files(roots):
    files = []
    for root in roots:
        files += glob.glob(os.path.join(root, "**", "test-reports", "TEST-*.xml"), recursive=True)
    return sorted(set(files))


def escape_annotation(text):
    # Workflow-command data must escape %, CR and LF.
    return text.replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A")


def main():
    roots = sys.argv[1:] or ["target"]
    files = report_files(roots)
    totals = {"tests": 0, "failures": 0, "errors": 0, "skipped": 0}
    failed = []  # (suite, test, kind, message, trace)
    unreadable = []
    for path in files:
        try:
            suite = ET.parse(path).getroot()
        except ET.ParseError as e:
            unreadable.append(f"{path}: {e}")
            continue
        for key in totals:
            totals[key] += int(suite.get(key, "0") or 0)
        for case in suite.iter("testcase"):
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

    out = []
    status = "failed" if failed else "passed"
    out.append(f"### Tests {status}: {totals['tests']} run, "
               f"{totals['failures']} failed, {totals['errors']} errors, "
               f"{totals['skipped']} skipped ({len(files)} suites)\n")
    if unreadable:
        out.append("Unreadable reports:\n")
        out += [f"- `{u}`" for u in unreadable]
        out.append("")
    if not files:
        out.append("No test reports found (the tests may not have started).\n")
    for suite, test, kind, message, trace in failed:
        out.append(f"#### {kind}: `{test}`\n")
        out.append(f"Suite `{suite}`\n")
        out.append("```text")
        out.append(message if message else "(no message)")
        if trace and trace != message:
            out.append("")
            out.append(trace)
        out.append("```\n")
        print(f"::error title={escape_annotation(test)}::{escape_annotation(message or kind)}")

    text = "\n".join(out) + "\n"
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        with open(summary, "a", encoding="utf-8") as f:
            f.write(text)
    else:
        sys.stdout.write(text)
    return 0


if __name__ == "__main__":
    sys.exit(main())
