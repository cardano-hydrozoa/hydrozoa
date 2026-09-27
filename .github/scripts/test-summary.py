#!/usr/bin/env python3
"""Summarise a CI job's test results from the build's structured result files.

Usage: test-summary.py [--json FILE] [ROOT ...]   (default ROOT: target). Reads, under each root,
  - test events:        **/ci-events/tests-*.jsonl
  - compile problems:   **/ci-events/compile-*.jsonl
  - dead letters:       **/ci-events/dead-letters.tsv
  - JUnit report NAMES: **/test-reports/TEST-*.xml (names only, as a cross-check; never parsed)
and skips any file with a directory named `ci-canary` between the root and itself: that is the
reporting canary's own project, and the canary check passes its snapshot directory as the root.
It also reads the test steps' outcomes from $TEST_STEPS ("unit=success integration=failure";
skipped steps didn't run by design and are left out), and the sbt version from
./project/build.properties.

Writes a Markdown summary to $GITHUB_STEP_SUMMARY (stdout when unset), workflow-command
annotations to stdout, and with --json the result below to FILE. Exit status is always 0: the
test steps decide pass or fail, and this only reports.

FORMATS, version 1 (the canonical description; the Scala writers point here)
============================================================================

Dead letters: `hydrozoa.dead-letters` v1, a text format described after them.

Two JSON-lines formats: one JSON object per line, UTF-8, `\\n`-terminated, each line flushed as
written. Every object has "type" and "time" (epoch milliseconds). Readers must reject a file whose
header names another schema or an unknown version, and must treat an unknown "type" or "status" as
not-passed.

Test events: `hydrozoa.test-events` v1. Written by a test-framework wrapper inside each forked
test JVM, one file per JVM: `<project's Test/target>/ci-events/tests-<pid>.jsonl` (sbt 2 puts that
under `target/out/jvm/scala-<v>/<project>/...`; readers glob `target/**/ci-events/tests-*.jsonl`).
Lines, in order of writing (suites run in parallel, so lines of different suites interleave):
 1. header: exactly once, first line:
    {"type":"header","time":...,"schema":"hydrozoa.test-events","version":1,"pid":12345,
     "project":"core","sbt":"2.0.1","testInterface":"1.0","java":"25.0.4.1"}
 2. runner-start: once per test framework the JVM runs:
    {"type":"runner-start","time":...,"runner":1,"framework":"ScalaTest",
     "frameworkClass":"org.scalatest.tools.Framework","frameworkVersion":"3.2.19"}
 3. suite-start: when a suite's task starts executing; "task" is unique within the file:
    {"type":"suite-start","time":...,"runner":1,"task":7,"suite":"hydrozoa.FooTest"}
 4. event: one per sbt test-interface event reported by the framework:
    {"type":"event","time":...,"runner":1,"task":7,"suite":"hydrozoa.FooTest",
     "status":"Success","selector":{"kind":"test","test":"does X"},"durationMs":12,
     "throwable":null}
    - status: the sbt.testing.Status name: Success, Error, Failure, Skipped, Ignored, Canceled,
      Pending. Any other value is a future status and counts as not-passed.
    - selector.kind: test (with test), nested-test (with suite, test), suite, nested-suite (with
      suite), wildcard (with test), other (with text).
    - durationMs: as reported; -1 or negative means unknown. Don't rely on it.
    - throwable: null or {"class":"java.lang.AssertionError","message":"...","trace":"..."};
      message cut after 16,000 chars and trace after 32,000 when written, each followed by a
      note of how many chars were cut.
 5. suite-end: when that task's execute returns or throws ("threw": a throwable as above, or null):
    {"type":"suite-end","time":...,"runner":1,"task":7,"suite":"hydrozoa.FooTest","threw":null}
 6. run-end: when the framework's runner is done:
    {"type":"run-end","time":...,"runner":1,"summary":"..."}
A suite that hangs or whose JVM dies leaves a suite-start without suite-end; a JVM that dies leaves
a runner-start without run-end. A hard kill may lose the line being written.

Compile problems: `hydrozoa.compile-problems` v1. Written by a compiler-reporter wrapper in the
build (sbt's JVM), one file per project and configuration:
`<project's target>/ci-events/compile-<config>.jsonl`, started over with a header the first time a
compilation calls the reporter, so it holds the problems of the latest compilation that reported
any (a compilation with nothing to report may not call it).
 1. header: {"type":"header","time":...,"schema":"hydrozoa.compile-problems","version":1,
             "project":"core","config":"test","sbt":"2.0.1","scala":"3.3.7"}
 2. problem: {"type":"problem","time":...,"severity":"Error","category":"...","code":"7",
              "message":"...","file":"src/main/scala/.../Foo.scala","line":12,"column":5}
    severity is Error, Warn or Info; code is the compiler's diagnostic code as it reports it
    (Scala 3's E007 arrives as "7"), or null; message is plain text (terminal colour codes
    removed) and may span lines; category may be empty; file is relative to the build's base
    directory when inside it, else absolute; line/column are 1-based, null when the problem has
    no position.
A compile served from sbt 2's cache starts no compilation, so its file keeps the previous
compile's problems: readers use compile problems only to explain a test step that failed, and
otherwise note them as stale.

Dead letters: `hydrozoa.dead-letters` v1. Written by the CI logging configs
(src/test/resources/logback-core-ci.xml, integration/src/test/resources/logback-ci.xml) inside
each forked test JVM, appended to `<project's Test/target>/ci-events/dead-letters.tsv`. Each JVM
that opens the file writes the header line `#hydrozoa.dead-letters<TAB>1`; every other line is one
dead letter: the level, a tab, and the message (`<message class> to <recipient path>[ from <sender
path>] (system running|system stopping|recipient crashed on purpose)`), tabs and newlines
flattened. WARN means the actor system was running; DEBUG that the dead letter was expected (the
system stopping, or a test having crashed the recipient on purpose). A file whose first line isn't
that header, or a line with another level, is invalid.

HOW THIS READER JUDGES (never "passed" without positive evidence; unknown input is never a pass)
===============================================================================================

- A file whose first line is not a header naming the schema its file name implies, with version
  the JSON integer 1, is INVALID and its contents are not read. A malformed line (not UTF-8, not
  JSON, not an object; an empty line) that ends in its `\\n` makes the file INVALID; its other
  lines are still read, so failures stay visible. So do structural violations: a second header, a
  duplicate runner or task, an event or suite-end for a task that isn't running, a suite-start for
  an unknown runner. A test-events file whose last line lacks its `\\n` was cut off by a kill: DID
  NOT FINISH (in a compile file, a note). Colour codes (ESC[...m) are dropped from all text.
- Per test-events file: every runner-start needs its run-end and every suite-start its suite-end,
  else DID NOT FINISH, naming the suites and the JVM. A file with no runner-start at all (the JVM
  died after its header) DID NOT FINISH too.
- Failure or Error, or a suite-end whose "threw" is set: FAILED. Canceled: counted and listed,
  never a failure by itself and never counted as passed. Ignored/Skipped/Pending: counted. Any other
  status, or an unknown record type: UNKNOWN RESULTS.
- Version guard: project/build.properties' sbt.version must equal TESTED_SBT, and every header's
  "sbt" must equal it ("unknown" does not), else VERSION NOT VERIFIED.
- Steps: a step that didn't succeed, with no failed and no unfinished test to show for it: DID NOT
  COMPLETE. When a step didn't succeed, compile files' Error problems are shown and annotated
  (COMPILE FAILED): Error problems in a file mean that configuration's latest real compile failed.
  When no step failed they can only be stale (a cache-served compile after a failed one; local
  runs), and are noted, never reported as errors.
- A step ran but there is no test-events file: NO TEST EVENTS. No step ran: NO STEP RAN. Events
  but not one Success, and nothing else wrong: NO TEST PASSED. Otherwise: PASSED.
- JUnit cross-check: the TEST-<suite>.xml names against the suites with a suite-end. A suite with
  a JUnit report but no suite-end ran without being recorded, so the events can't vouch for it:
  UNRECORDED SUITES. A suite-end without a JUnit report is a warning, shown on the verdict line.

GitHub shows at most 10 error annotations per step: past 10, the first 9 are emitted and then one
line saying how many more the summary holds. The summary stays under SUMMARY_BUDGET (GitHub drops
one over 1 MiB whole): each item's text is capped, and past half the budget items are listed by
name only, then counted.

THE --json RESULT (`hydrozoa.test-summary` v1; for the reporting canary, which compares it exactly)
===================================================================================================

One JSON object. Names are as written (colour codes dropped); files are relative to the current
directory; lists are in file order (files sorted by path) unless marked sorted. A field described
by the things it counts holds their number.
  schema, version        "hydrozoa.test-summary", 1
  verdict                "passed", or the first of states
  states                 ["passed"], or every not-passed state, in this order: "failed",
                         "did-not-finish", "did-not-complete", "compile-failed", "invalid-input",
                         "version-not-verified", "unknown-results", "no-test-events",
                         "no-step-ran", "no-test-passed", "unrecorded-suites"
  warnings               ["junit-mismatch"] or []: a suite-end without a JUnit report
  steps                  {step: outcome} for the steps that ran
  passed, failed, errors, cancelled, ignored, skipped, pending
                         events with status Success, Failure, Error, Canceled, Ignored, Skipped,
                         Pending
  unknown                len(unknownResults)
  failures               [{suite, test, status}]: every Failure and Error event, and every
                         suite-end that threw (status "SuiteThrew", test null)
  cancelledTests         [{suite, test}]
  unknownResults         [{file, type, status, suite, test}]: an event with another status
                         (type "event"), or a record of another type (status/suite/test null)
  suitesFinished, suitesThrew
                         suite-ends, and those whose "threw" is set
  unfinishedSuites       sorted names of the suites with a suite-start and no suite-end
  unfinishedRunners      runner-starts without a run-end
  unfinishedJvms         [{file, pid, project, runners: [framework of each unended runner],
                         noRunner: bool, truncated: bool}] for each test-events file that did not
                         finish
  jvms                   test-events files with a valid header
  compileErrors          [{file, line, column, code, category, message}]: every problem whose
                         severity isn't Warn or Info, from every compile file with a valid
                         header, live or stale
  compileErrorsLive      true when a step didn't succeed (the errors are shown and annotated)
  compileWarnings        problems with severity Warn or Info
  invalidFiles           [{file, reasons: [str]}]
  versionErrors          [str]
  buildSbt, testedSbt    project/build.properties' sbt.version (null if unreadable), TESTED_SBT
  junitOnly, eventsOnly  sorted suite names with a JUnit report and no suite-end, and the reverse
  excludedCanaryFiles    files skipped for a `ci-canary` directory
  deadLettersRunning     dead letters at WARN (sent while an actor system ran)
  deadLettersExpected    dead letters at DEBUG (the system stopping, or a recipient crashed on
                         purpose)
  deadLetterFilesInvalid [str]: a reason per dead-letters file that couldn't be read
  notes                  [str]
The three deadLetter fields were added within v1: they are new fields, and no existing field
changed meaning, so a reader of the earlier v1 is unaffected.

"""

import collections
import glob
import json
import os
import re
import sys

TESTED_SBT = "2.0.1"
TESTS_SCHEMA = "hydrozoa.test-events"
COMPILE_SCHEMA = "hydrozoa.compile-problems"
# Each counted status and its name in the --json result.
STATUSES = {
    "Success": "passed",
    "Failure": "failed",
    "Error": "errors",
    "Canceled": "cancelled",
    "Ignored": "ignored",
    "Skipped": "skipped",
    "Pending": "pending",
}
CANARY_DIR = "ci-canary"
TRACE_LINES = 12
DETAIL_CHARS = 8_000
DEAD_LETTERS_HEADER = "#hydrozoa.dead-letters\t1"
DEAD_LETTERS_SHOWN = 20
SUMMARY_BUDGET = 512 * 1024
DETAIL_BUDGET = SUMMARY_BUDGET // 2
TITLE_CHARS = 200
ANNOTATION_CHARS = 4_000
MAX_ERRORS = 10  # GitHub displays at most 10 error annotations per step
ANSI = re.compile("\x1b\\[[0-9;]*m")
VERDICT = None  # for the top-level guard's message


# --- text helpers ---------------------------------------------------------------------------


def clean(v):
    """Drops colour codes and replaces lone surrogates (valid JSON, unencodable) in a value."""
    if isinstance(v, str):
        return ANSI.sub("", v).encode("utf-8", "replace").decode("utf-8")
    if isinstance(v, dict):
        return {clean(k): clean(x) for k, x in v.items()}
    if isinstance(v, list):
        return [clean(x) for x in v]
    return v


def s(v):
    return "" if v is None else v if isinstance(v, str) else json.dumps(v)


def is_int(v):
    return type(v) is int  # not bool, not float


def cap(text, limit):
    """At most `limit` chars, the note of what was cut included."""
    if len(text) <= limit:
        return text
    note = f"\n... ({len(text)} chars in all)"
    return text[: limit - len(note)] + note


def one_line(text):
    return text.replace("\r\n", "\\n").replace("\n", "\\n").replace("\r", "\\r")


def code(text, limit=300):
    """An inline code span that no backtick in `text` can close."""
    text = one_line(cap(s(text), limit))
    fence = "`" * (max((len(m) for m in re.findall("`+", text)), default=0) + 1)
    return f"{fence} {text} {fence}"


def fenced(text):
    fence = "`" * max(3, max((len(m) for m in re.findall("`+", text)), default=0) + 1)
    return f"{fence}text\n{text}\n{fence}"


def esc_data(text):
    # Workflow-command data must escape %, CR and LF.
    return text.replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A")


def esc_prop(text):
    # A property value must also escape the `:` and `,` that delimit properties.
    return esc_data(text).replace(":", "%3A").replace(",", "%2C")


def annotation(level, message, title=None, **props):
    if title is not None:
        title = one_line(title)
        props["title"] = title if len(title) <= TITLE_CHARS else title[: TITLE_CHARS - 3] + "..."
    head = ",".join(f"{k}={esc_prop(str(v))}" for k, v in props.items())
    return f"::{level} {head}::{esc_data(cap(message, ANNOTATION_CHARS))}"


def rel(path):
    return os.path.relpath(path)


# --- reading --------------------------------------------------------------------------------


def find(roots, pattern, excluded):
    found = set()
    for root in roots:
        for f in glob.glob(os.path.join(glob.escape(root), "**", pattern), recursive=True):
            if not os.path.isfile(f):
                continue
            if CANARY_DIR in os.path.relpath(f, root).split(os.sep)[:-1]:
                excluded.add(os.path.realpath(f))
            else:
                found.add(os.path.realpath(f))
    return sorted(found)


def read_jsonl(path, schema):
    """Returns (header or None if rejected, records, problems, truncated, why-rejected)."""
    try:
        with open(path, "rb") as f:
            lines = f.read().split(b"\n")
    except OSError as e:
        return None, [], [], False, f"unreadable ({type(e).__name__})"
    tail = lines.pop()  # b"" when the file ends with its newline
    records, problems, first = [], [], {}
    for n, raw in enumerate(lines + ([tail] if tail else []), 1):
        try:
            obj = clean(json.loads(raw.decode("utf-8")))
            if not isinstance(obj, dict):
                raise ValueError("not a JSON object")
        except Exception as e:  # noqa: BLE001 - any undecodable line
            if n == 1:
                break
            if n <= len(lines):  # a complete line; a partial last one is the kill's
                problems.append(f"line {n} is malformed ({type(e).__name__})")
            continue
        if n == 1:
            first = obj
        else:
            records.append(obj)
    if first.get("type") != "header" or first.get("schema") != schema:
        return None, [], [], bool(tail), f"its first line is not a {schema} header"
    if not (is_int(first.get("version")) and first["version"] == 1):
        return None, [], [], bool(tail), f"schema version {s(first.get('version'))}, not 1"
    return first, records, problems, bool(tail), None


def read_dead_letters(paths):
    """Dead letters from `hydrozoa.dead-letters` files: (messages sent while running, how many
    were expected, one reason per invalid file)."""
    running, stopping, invalid = [], 0, []
    for path in paths:
        try:
            with open(path, encoding="utf-8", errors="replace") as f:
                lines = f.read().splitlines()
        except OSError as e:
            invalid.append(f"{rel(path)}: can't read ({type(e).__name__}: {e})")
            continue
        if lines and lines[0] != DEAD_LETTERS_HEADER:
            invalid.append(f"{rel(path)}: first line is {lines[0][:80]!r}, not the v1 header")
            continue
        for line in lines:
            level, _, message = line.partition("\t")
            if line == DEAD_LETTERS_HEADER:
                continue
            if level == "WARN":
                running.append(message)
            elif level == "DEBUG":
                stopping += 1
            else:
                invalid.append(f"{rel(path)}: a line with level {level[:20]!r}")
                break
    return running, stopping, invalid


def build_sbt_version():
    try:
        with open(os.path.join("project", "build.properties"), encoding="utf-8") as f:
            for line in f:
                m = re.match(r"\s*sbt\.version\s*[=:]\s*(\S+)", line)
                if m:
                    return m.group(1)
    except (OSError, ValueError):
        pass
    return None


def step_outcomes(text):
    steps = {}
    for item in (text or "").split():
        name, _, outcome = item.partition("=")
        if outcome != "skipped":
            steps[name] = outcome or "unknown"
    return steps


def test_name(sel):
    if not isinstance(sel, dict):
        return "(no selector)"
    kind = sel.get("kind")
    if kind in ("test", "wildcard"):
        return s(sel.get("test"))
    if kind == "nested-test":
        return f"{s(sel.get('suite'))} / {s(sel.get('test'))}"
    if kind == "suite":
        return "(the suite)"
    if kind == "nested-suite":
        return f"(nested suite {s(sel.get('suite'))})"
    if kind == "other":
        return s(sel.get("text"))
    return f"(selector {json.dumps(sel)[:200]})"


class Results:
    def __init__(self):
        self.counts = dict.fromkeys(STATUSES, 0)
        self.failures, self.cancelled, self.unknown, self.invalid = [], [], [], []
        self.unfinished_suites, self.unfinished_jvms, self.compile, self.notes = [], [], [], []
        self.finished_suites = set()
        self.suites_finished = self.suites_threw = self.jvms = 0
        self.sbt_seen = {}  # sbt version -> [files]

    def reject(self, path, reasons):
        self.invalid.append({"file": rel(path), "reasons": reasons})

    def odd(self, path, kind, status=None, suite=None, test=None):
        self.unknown.append(
            {"file": rel(path), "type": kind, "status": status, "suite": suite, "test": test}
        )

    def read_tests(self, path):
        header, records, problems, truncated, why = read_jsonl(path, TESTS_SCHEMA)
        if header is None:
            return self.reject(path, [f"rejected: {why}"])
        self.jvms += 1
        self.sbt_seen.setdefault(s(header.get("sbt")), []).append(path)
        jvm = {"file": rel(path), "pid": header.get("pid"), "project": s(header.get("project"))}
        runners, run_ends, starts, ends = {}, set(), {}, set()
        for r in records:
            kind, run, task = r.get("type"), s(r.get("runner")), s(r.get("task"))
            suite = s(r.get("suite"))
            if kind == "runner-start":
                if run in runners:
                    problems.append(f"runner {run} started twice")
                runners[run] = s(r.get("framework"))
            elif kind == "run-end":
                if run not in runners or run in run_ends:
                    problems.append(f"run-end for runner {run}, not started or already ended")
                run_ends.add(run)
            elif kind == "suite-start":
                if task in starts:
                    problems.append(f"task {task} started twice")
                if run not in runners:
                    problems.append(f"suite-start of {suite} for unknown runner {run}")
                starts[task] = r
            elif kind == "event":
                if task not in starts or task in ends:
                    problems.append(f"event in {suite} for task {task}, not running")
                status, test = r.get("status"), test_name(r.get("selector"))
                if isinstance(status, str) and status in self.counts:
                    self.counts[status] += 1
                else:
                    self.odd(path, "event", status, suite, test)
                if status in ("Failure", "Error"):
                    self.failures.append(
                        dict(
                            jvm, suite=suite, test=test, status=status, throwable=r.get("throwable")
                        )
                    )
                elif status == "Canceled":
                    self.cancelled.append({"suite": suite, "test": test})
            elif kind == "suite-end":
                if task not in starts or task in ends:
                    problems.append(f"suite-end of {suite} for task {task}, not running")
                ends.add(task)
                self.suites_finished += 1
                self.finished_suites.add(suite)
                if r.get("threw") is not None:
                    self.suites_threw += 1
                    self.failures.append(
                        dict(jvm, suite=suite, test=None, status="SuiteThrew", throwable=r["threw"])
                    )
            elif kind == "header":
                problems.append("a second header")
            else:
                self.odd(path, s(kind))
        times = [r["time"] for r in records if is_int(r.get("time"))]
        for task, r in starts.items():
            if task not in ends:
                ran = max(times) - r["time"] if times and is_int(r.get("time")) else None
                self.unfinished_suites.append(dict(jvm, suite=s(r.get("suite")), ranMs=ran))
        lost = [runners[run] for run in runners if run not in run_ends]
        if lost or not runners or truncated:
            self.unfinished_jvms.append(
                dict(jvm, runners=lost, noRunner=not runners, truncated=truncated)
            )
        if problems:
            self.reject(path, problems)

    def read_compile(self, path):
        header, records, problems, truncated, why = read_jsonl(path, COMPILE_SCHEMA)
        if header is None:
            return self.reject(path, [f"rejected: {why}"])
        self.sbt_seen.setdefault(s(header.get("sbt")), []).append(path)
        if problems:
            self.reject(path, problems)
        if truncated:
            self.notes.append(f"{rel(path)} ends mid-line: sbt was killed while compiling.")
        for r in records:
            if r.get("type") == "problem":
                f = s(r.get("file"))
                if os.path.isabs(f) and not rel(f).startswith(".."):
                    f = rel(f)
                title = s(r.get("category")) or "Compile error"
                if r.get("code") not in (None, ""):  # Scala 3 reports E007 as "7"
                    num = s(r.get("code"))
                    scala3 = s(header.get("scala")).startswith("3.") and num.isdigit()
                    title += f" [E{int(num):03d}]" if scala3 else f" [{num}]"
                p = {k: r.get(k) for k in ("line", "column", "code", "category")}
                p.update(severity=s(r.get("severity")), file=f, message=s(r.get("message")))
                self.compile.append(dict(p, title=title))
            elif r.get("type") != "header":
                self.odd(path, s(r.get("type")))


# --- rendering ------------------------------------------------------------------------------


def failure_text(f):
    """(annotation title, annotation message, name line, detail block) for a failure."""
    th = f["throwable"]
    if isinstance(th, dict):
        cls, msg, trace = s(th.get("class")), s(th.get("message")), s(th.get("trace"))
    else:
        cls, msg, trace = "", "", ""
    head = (f"{cls}: {msg}" if msg else cls) or "(no throwable reported)"
    rest = trace[len(head) :] if trace.startswith(head) else trace
    detail = cap("\n".join([head, *rest.strip("\n").splitlines()[:TRACE_LINES]]), DETAIL_CHARS)
    kind = "Suite threw" if f["status"] == "SuiteThrew" else f["status"]
    test = "(the suite's execution)" if f["test"] is None else f["test"]
    line = f"- {kind}: {code(test)} in {code(f['suite'])}"
    where = f"project {code(f['project'])}, JVM {code(f['pid'])}"
    block = f"#### {kind}: {code(test)}\n\nSuite {code(f['suite'])} ({where})\n\n{fenced(detail)}\n"
    return test, f"{f['suite']}: {msg or cls or kind}", line, block


def position(p):
    line, col = p["line"], p["column"]
    return p["file"] + (f":{line}" if is_int(line) else "") + (f":{col}" if is_int(col) else "")


def plain(text):
    return text


def unknown_text(u, fmt=plain):
    if u["type"] == "event":
        return f"status {fmt(s(u['status']) or '(none)')}: {fmt(u['test'])} in {fmt(u['suite'])}"
    return f"record type {fmt(u['type'])} in {fmt(u['file'])}"


def suite_text(u, fmt=plain):
    ran = "" if u["ranMs"] is None else f", started {u['ranMs'] / 1000:.0f} s before the file's end"
    where = f"project {fmt(u['project'])}, JVM {fmt(s(u['pid']))}"
    return f"suite {fmt(u['suite'])} did not finish{ran} ({where})"


def jvm_text(j, fmt=plain):
    why = [f"runner {fmt(r)} never ended" for r in j["runners"]]
    if j["noRunner"]:
        why.append("no test framework ever started (died after its header?)")
    if j["truncated"]:
        why.append("the file ends mid-line (killed while writing)")
    where = f"project {fmt(j['project'])}, {fmt(j['file'])}"
    return f"JVM {fmt(s(j['pid']))} ({where}): " + "; ".join(why)


class Out:
    def __init__(self):
        self.parts, self.size = [], 0

    def add(self, text):
        self.parts.append(text)
        self.size += len(text.encode("utf-8", "replace")) + 1

    def section(self, title, items):
        """items: (name line, detail block or None). Details, then names, then a count."""
        if not items:
            return
        self.add(f"#### {title} ({len(items)})\n")
        for i, (line, detail) in enumerate(items):
            if detail and self.size + len(detail.encode("utf-8", "replace")) < DETAIL_BUDGET:
                self.add(detail)
            elif self.size + len(line.encode("utf-8", "replace")) < SUMMARY_BUDGET - 32_768:
                self.add(line)
            else:
                self.add(f"- ... and {len(items) - i} more, not listed (the summary's size budget)")
                break
        self.add("")

    def text(self):
        text = "\n".join(self.parts) + "\n"
        data = text.encode("utf-8", "replace")
        if len(data) > SUMMARY_BUDGET:  # a safety net; the sections keep well under it
            text = data[: SUMMARY_BUDGET - 200].decode("utf-8", "ignore") + "\n\n(cut: size)\n"
        return text


# --- the verdict ----------------------------------------------------------------------------


def main():
    global VERDICT
    args = sys.argv[1:]
    json_path = None
    if args[:1] == ["--json"]:
        json_path, args = args[1], args[2:]
    roots = args or ["target"]
    steps = step_outcomes(os.environ.get("TEST_STEPS"))
    not_ok = [f"{n} {o}" for n, o in steps.items() if o != "success"]
    res, excluded = Results(), set()
    test_files = find(roots, os.path.join("ci-events", "tests-*.jsonl"), excluded)
    for path in test_files:
        res.read_tests(path)
    for path in find(roots, os.path.join("ci-events", "compile-*.jsonl"), excluded):
        res.read_compile(path)
    dead, dead_expected, dead_invalid = read_dead_letters(
        find(roots, os.path.join("ci-events", "dead-letters.tsv"), excluded)
    )
    xml_files = find(roots, os.path.join("test-reports", "TEST-*.xml"), excluded)
    xml_suites = {os.path.basename(f)[len("TEST-") : -len(".xml")] for f in xml_files}
    only_xml = sorted(xml_suites - res.finished_suites)
    only_ev = sorted(res.finished_suites - xml_suites)
    errs = [p for p in res.compile if p["severity"] not in ("Warn", "Info")]
    live = bool(not_ok)  # compile errors can explain a step that didn't succeed

    # errors, cautions and warnings hold (title, message) pairs: error annotations, a state's
    # warning annotations, and other warnings.
    states, errors, cautions, warnings = [], [], [], []

    def state(name, title=None, message=None, level=errors):
        states.append(name)
        if title:
            level.append((title, message))

    built = build_sbt_version()
    versions = []
    if built is None:
        versions.append("can't read sbt.version from project/build.properties")
    elif built != TESTED_SBT:
        versions.append(
            f"reporting not re-verified for sbt {built} (tested with {TESTED_SBT}): "
            "run the reporting canary and update TESTED_SBT"
        )
    for seen, paths in sorted(res.sbt_seen.items()):
        if seen != (built or TESTED_SBT):
            versions.append(
                f"{len(paths)} result files were written under sbt {seen or '(none)'}, but "
                f"project/build.properties says {built}: " + ", ".join(rel(p) for p in paths[:5])
            )
    errors += [("Reporting not verified", v) for v in versions]

    if res.failures:
        state("FAILED")
    if res.unfinished_suites or res.unfinished_jvms:
        state("DID NOT FINISH")
    if not_ok and not states:
        why = "See the compile errors." if errs else "See the step's log."
        state(
            "DID NOT COMPLETE",
            "Tests did not complete",
            f"{', '.join(not_ok)}, and no failed or unfinished test was recorded. {why}",
        )
    if live and errs:
        state("COMPILE FAILED")
    if res.invalid:
        state(
            "INVALID INPUT",
            "Invalid result files",
            f"{len(res.invalid)} result files are invalid; see the job summary",
        )
    if versions:
        state("VERSION NOT VERIFIED")
    if res.unknown:
        state("UNKNOWN RESULTS")
    if steps and not test_files:
        ran = ", ".join(f"{n} {o}" for n, o in steps.items())
        state(
            "NO TEST EVENTS",
            "No test events",
            f"a step ran ({ran}) but no ci-events/tests-*.jsonl was found under {', '.join(roots)}",
        )
    if not steps:
        state(
            "NO STEP RAN",
            "No step ran",
            f"TEST_STEPS lists no step that ran: {os.environ.get('TEST_STEPS')!r}",
            level=cautions,
        )
    if not states and res.counts["Success"] == 0:
        state("NO TEST PASSED", "No test passed", "the test events hold no passed test")
    if only_xml:
        state(
            "UNRECORDED SUITES",
            "Suites ran unrecorded",
            f"{len(only_xml)} suites have a JUnit report but no suite-end in the test events, so "
            "they ran without being recorded; see the job summary",
        )
    if dead_invalid:
        warnings.append(("Dead-letter files unreadable", "; ".join(dead_invalid)))
    if only_ev:
        warnings.append(
            (
                "JUnit reports disagree with the test events",
                f"{len(only_ev)} suites have a suite-end but no JUnit report; see the job summary",
            )
        )
    notes = list(res.notes)
    if errs and not live:
        notes.append(
            f"The compile files hold {len(errs)} errors from an earlier compile: no step "
            "failed, so they are stale (a compile served from sbt's cache rewrites nothing)."
        )
    warns = len(res.compile) - len(errs)
    if warns:
        notes.append(f"The compile files hold {warns} warnings or infos.")
    if excluded:
        notes.append(f"Skipped {len(excluded)} files under a {CANARY_DIR} directory.")

    c = res.counts
    verdict = ", ".join(states) or "PASSED" + (
        f" ({c['Canceled']} cancelled)" if c["Canceled"] else ""
    )
    if warnings:
        verdict += f", with {len(warnings)} warning{'s' if len(warnings) > 1 else ''}"
    VERDICT = verdict

    # Annotations first: they survive a summary that can't be written.
    failures = [failure_text(f) for f in res.failures]
    items = [(t, m, {}) for t, m in errors] + [(t, m, {}) for t, m, _, _ in failures]
    items += [("Did not finish", suite_text(u), {}) for u in res.unfinished_suites]
    items += [("Did not finish", jvm_text(j), {}) for j in res.unfinished_jvms]
    for p in errs if live else []:
        props = {"file": p["file"]} if p["file"] else {}
        if is_int(p["line"]):
            props["line"] = p["line"]
            if is_int(p["column"]):
                props["col"] = p["column"]
        items.append((p["title"], p["message"], props))
    items += [("Unknown result", unknown_text(u), {}) for u in res.unknown]
    shown = items if len(items) <= MAX_ERRORS else items[: MAX_ERRORS - 1]
    for title, message, props in shown:
        print(annotation("error", message, title, **props))
    if len(items) > len(shown):
        more = len(items) - len(shown)
        print(
            annotation(
                "error",
                f"{more} more errors (failures, unfinished suites, compile "
                "errors) — see the job summary",
            )
        )
    for title, message in cautions + warnings:
        print(annotation("warning", message, title))

    if json_path:
        codes = [x.lower().replace(" ", "-") for x in states] or ["passed"]
        result = {
            "schema": "hydrozoa.test-summary", "version": 1, "verdict": codes[0],
            "states": codes, "warnings": ["junit-mismatch"] * bool(warnings), "steps": steps,
            **{STATUSES[k]: v for k, v in c.items()}, "unknown": len(res.unknown),
            "failures": [{k: f[k] for k in ("suite", "test", "status")} for f in res.failures],
            "cancelledTests": res.cancelled, "unknownResults": res.unknown,
            "suitesFinished": res.suites_finished, "suitesThrew": res.suites_threw,
            "unfinishedSuites": sorted(u["suite"] for u in res.unfinished_suites),
            "unfinishedRunners": sum(len(j["runners"]) for j in res.unfinished_jvms),
            "unfinishedJvms": res.unfinished_jvms, "jvms": res.jvms,
            "compileErrors": [{k: p[k] for k in ("file", "line", "column", "code", "category",
                                                   "message")} for p in errs],
            "compileErrorsLive": live, "compileWarnings": warns, "invalidFiles": res.invalid,
            "versionErrors": versions, "buildSbt": built, "testedSbt": TESTED_SBT,
            "junitOnly": only_xml, "eventsOnly": only_ev, "excludedCanaryFiles": len(excluded),
            "deadLettersRunning": len(dead), "deadLettersExpected": dead_expected,
            "deadLetterFilesInvalid": dead_invalid,
            "notes": notes,
        }  # fmt: skip
        try:
            with open(json_path, "w", encoding="utf-8", errors="replace") as f:
                f.write(json.dumps(result, indent=1, ensure_ascii=False) + "\n")
        except OSError as e:  # the canary's problem; the summary still gets written
            why = f"can't write {json_path} ({type(e).__name__}: {e})"
            print(annotation("warning", why, "Test summary JSON"))
            notes.append(why)

    out = Out()
    out.add(f"### Tests: {verdict}\n")
    out.add(
        f"Tests: {c['Success']} passed, {c['Failure']} failed, {c['Error']} errors, "
        f"{c['Canceled']} cancelled, {c['Ignored']} ignored, {c['Skipped']} skipped, "
        f"{c['Pending']} pending, {len(res.unknown)} unknown. Suites: {res.suites_finished} "
        f"finished ({res.suites_threw} threw), {len(res.unfinished_suites)} unfinished. "
        f"Test JVMs: {res.jvms}.\n"
    )
    if steps:
        out.add("Steps: " + ", ".join(f"{n} {o}" for n, o in steps.items()) + ".\n")
    out.add(
        f"Dead letters: {len(dead)} while an actor system was running, {dead_expected} expected "
        "(the system stopping, or a test crashing the recipient on purpose).\n"
    )
    if errors or cautions or warnings:
        told = errors + cautions + warnings
        out.add("\n".join(f"- **{t}**: {one_line(m)}" for t, m in told) + "\n")
    out.section("Failures", [(line, block) for _, _, line, block in failures])
    out.section(
        "Did not finish",
        [("- " + suite_text(u, code), None) for u in res.unfinished_suites]
        + [("- " + jvm_text(j, code), None) for j in res.unfinished_jvms],
    )
    lines = []
    for p in errs if live else []:
        first = p["message"].splitlines()[0] if p["message"] else ""
        name = f"- {code(position(p) or '(no position)')} {code(p['title'], 80)}: {code(first)}"
        lines.append((name, f"{name}\n\n{fenced(cap(p['message'], DETAIL_CHARS))}\n"))
    out.section("Compile errors", lines)
    out.section("Unknown results", [("- " + unknown_text(u, code), None) for u in res.unknown])
    out.section(
        "Invalid result files",
        [
            (f"- {code(i['file'])}: {code('; '.join(i['reasons']), 2000)}", None)
            for i in res.invalid
        ],
    )
    out.section(
        "Dead letters while an actor system was running",
        [
            (f"- {n}× {code(m)}", None)
            for m, n in collections.Counter(dead).most_common(DEAD_LETTERS_SHOWN)
        ],
    )
    out.section(
        "Cancelled tests (not failures, not passes)",
        [(f"- cancelled: {code(x['test'])} in {code(x['suite'])}", None) for x in res.cancelled],
    )
    out.section(
        "JUnit reports against test events",
        [(f"- JUnit report, no finished suite in the events: {code(n)}", None) for n in only_xml]
        + [(f"- finished in the events, no JUnit report: {code(n)}", None) for n in only_ev],
    )
    if notes:
        out.add("#### Notes\n\n" + "\n".join(f"- {one_line(n)}" for n in notes))

    text = out.text()
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        with open(summary, "a", encoding="utf-8", errors="replace") as f:
            f.write(text)
    else:
        sys.stdout.write(text)


if __name__ == "__main__":
    try:
        sys.stdout.reconfigure(errors="replace")
        main()
    except BaseException as e:  # never fail the job; say that the summary is missing, and why
        why = f"the summary failed ({type(e).__name__}: {e}); see the test steps' logs"
        if VERDICT:
            why = f"verdict {VERDICT}, but {why}"
        print(annotation("warning", why, "Test summary"))
        line = f"### Tests: summary failed. {one_line(why)}\n"
        try:
            with open(
                os.environ["GITHUB_STEP_SUMMARY"], "a", encoding="utf-8", errors="replace"
            ) as f:
                f.write(line)
        except BaseException:  # noqa: BLE001 - the summary file is what failed; use the log
            print(line, end="")
    sys.exit(0)
