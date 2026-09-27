// The reporter wrapping and the `${BASE}` and column handling follow
// github.com/sideeffffect/sbt-github-actions-logger (GHACompilerReporter, apiAdapter, Compat),
// Copyright 2013-2021 JetBrains s.r.o. and 2026 Ondra Pelech, under the Apache License 2.0.

// `compilerReporter` is `private[sbt]`, so this file lives in a package under `sbt` to reach it.
package sbt.hydrozoa

import java.io.{File, FileOutputStream, OutputStreamWriter, Writer}
import java.nio.charset.StandardCharsets
import sbt.*
import sbt.Keys.*
import xsbti.{Position, Problem, Reporter, Severity}

/** Records every compiler problem to a JSON-lines file, for CI's summary.
  *
  * The format is `hydrozoa.compile-problems` version 1, described in
  * `.github/scripts/test-summary.py`, which reads it. The file is
  * `<project's target>/ci-events/compile-<configuration>.jsonl`, and holds the problems of the
  * latest compilation that reported anything: it starts over, with a header, the first time a
  * compilation calls the reporter. A compilation served from sbt's cache calls nothing, so the file
  * can outlive the problems it lists; the summary only uses it to explain a test step that failed.
  *
  * It wraps sbt's own reporter, which still prints everything as before. `compilerReporter` is an
  * internal, experimental sbt key; if a future sbt stops calling it, the file stays empty, and the
  * reporting canary is what notices.
  */
object CompileProblems {
    val Schema: String = "hydrozoa.compile-problems"
    val Version: Int = 1

    /** The settings for one configuration (Compile or Test) of a project. */
    def settings(config: Configuration): Seq[Setting[?]] = Seq(
      config / compile / compilerReporter := Def.uncached {
          new Recording(
            (config / compile / compilerReporter).value,
            target.value / "ci-events" / s"compile-${config.name}.jsonl",
            (ThisBuild / baseDirectory).value,
            Seq(
              "project" -> thisProject.value.id,
              "config" -> config.name,
              "sbt" -> sbtVersion.value,
              "scala" -> scalaVersion.value
            )
          )
      }
    )

    private final class Recording(
        delegate: Reporter,
        file: File,
        base: File,
        header: Seq[(String, Any)]
    ) extends Reporter {
        private var out: Option[Writer] = None

        private def writer(): Writer = synchronized {
            out.getOrElse {
                file.getParentFile.mkdirs()
                val w = new OutputStreamWriter(new FileOutputStream(file), StandardCharsets.UTF_8)
                out = Some(w)
                write(w, Seq("type" -> "header", "schema" -> Schema, "version" -> Version) ++ header)
                w
            }
        }

        private def write(w: Writer, fields: Seq[(String, Any)]): Unit = synchronized {
            w.write(json(fields :+ ("time" -> System.currentTimeMillis())))
            w.write('\n')
            w.flush()
        }

        def reset(): Unit = { writer(); delegate.reset() }
        def hasErrors(): Boolean = delegate.hasErrors()
        def hasWarnings(): Boolean = delegate.hasWarnings()
        def printSummary(): Unit = delegate.printSummary()
        def problems(): Array[Problem] = delegate.problems()
        def comment(pos: Position, msg: String): Unit = delegate.comment(pos, msg)

        def log(problem: Problem): Unit = {
            val pos = problem.position()
            def opt(v: java.util.Optional[Integer]): Any = if v.isPresent then v.get.intValue else null
            write(
              writer(),
              Seq(
                "type" -> "problem",
                "severity" -> problem.severity().name(),
                "category" -> problem.category(),
                "message" -> problem.message(),
                "code" -> {
                    val c = problem.diagnosticCode()
                    if c.isPresent then c.get.code else null
                },
                "file" -> (if pos.sourcePath().isPresent then relative(pos.sourcePath().get) else null),
                "line" -> opt(pos.line()),
                // xsbti columns are 0-based; the format's are 1-based.
                "column" -> (if pos.pointer().isPresent then pos.pointer().get.intValue + 1 else null)
              )
            )
            delegate.log(problem)
        }

        // sbt reports some paths as "${BASE}/src/..."; the format wants them relative to the build.
        private def relative(path: String): String = {
            val p = path.replaceFirst("""^\$\{BASE\}/""", "")
            val abs = new File(p)
            if abs.isAbsolute then IO.relativize(base, abs).getOrElse(p) else p
        }
    }

    private def json(fields: Seq[(String, Any)]): String =
        fields.map { case (k, v) => quoted(k) + ":" + value(v) }.mkString("{", ",", "}")

    private def value(v: Any): String = v match {
        case null                      => "null"
        case n: (Int | Long)           => n.toString
        case s                         => quoted(String.valueOf(s))
    }

    // The compiler colours its messages for the terminal; the format holds plain text.
    private def quoted(s: String): String = {
        val b = new StringBuilder("\"")
        s.replaceAll("\u001b\\[[0-9;]*m", "").foreach {
            case '"'          => b.append("\\\"")
            case '\\'         => b.append("\\\\")
            case '\n'         => b.append("\\n")
            case '\r'         => b.append("\\r")
            case '\t'         => b.append("\\t")
            case c if c < ' ' => b.append(f"\\u${c.toInt}%04x")
            case c            => b.append(c)
        }
        b.append('"').toString
    }
}
