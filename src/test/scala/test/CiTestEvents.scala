package test

import java.io.{File, FileOutputStream, OutputStreamWriter, PrintWriter, StringWriter, Writer}
import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicInteger
import sbt.testing.*

/** Records every test event of this JVM to a JSON-lines file, for CI's summary.
  *
  * The format is `hydrozoa.test-events` version 1, described in `.github/scripts/test-summary.py`,
  * which reads it. Each line is written and flushed as it happens, so a suite that hangs or a JVM
  * that dies still leaves its `suite-start` behind; sbt's JUnit reports, by contrast, are written
  * only when a suite ends, and write a cancelled test with no status, so it reads as passed.
  *
  * Recording is on when the forked test JVM gets `-Dhydrozoa.ci.events=<dir>` (the build sets it
  * for the projects whose frameworks are wrapped in [[RecordingFramework]]); without it, the
  * wrappers only delegate. The file is `<dir>/tests-<pid>.jsonl`.
  */
object CiTestEvents {
    val Schema: String = "hydrozoa.test-events"
    val Version: Int = 1
    private val MessageChars = 16_000
    private val TraceChars = 32_000

    private val out: Option[Writer] =
        Option(System.getProperty("hydrozoa.ci.events")).map { dir =>
            val pid = ProcessHandle.current().pid()
            new File(dir).mkdirs()
            val file = new File(dir, s"tests-$pid.jsonl")
            val w = new OutputStreamWriter(new FileOutputStream(file), StandardCharsets.UTF_8)
            write(
              w,
              "header",
              "schema" -> Schema,
              "version" -> Version,
              "pid" -> pid,
              "project" -> prop("hydrozoa.ci.project"),
              "sbt" -> prop("hydrozoa.ci.sbt"),
              "testInterface" -> versionOf(classOf[Framework]),
              "java" -> System.getProperty("java.version")
            )
            w
        }

    private val runners = new AtomicInteger
    private val tasks = new AtomicInteger

    def enabled: Boolean = out.isDefined

    def runnerStart(framework: Framework, implementation: Class[?]): Int = {
        val id = runners.incrementAndGet()
        emit(
          "runner-start",
          "runner" -> id,
          "framework" -> framework.name(),
          "frameworkClass" -> implementation.getName,
          "frameworkVersion" -> versionOf(implementation)
        )
        id
    }

    def suiteStart(runner: Int, suite: String): Int = {
        val id = tasks.incrementAndGet()
        emit("suite-start", "runner" -> runner, "task" -> id, "suite" -> suite)
        id
    }

    def event(runner: Int, task: Int, e: Event): Unit =
        emit(
          "event",
          "runner" -> runner,
          "task" -> task,
          "suite" -> e.fullyQualifiedName(),
          "status" -> e.status().name(),
          "selector" -> selector(e.selector()),
          "durationMs" -> e.duration(),
          "throwable" -> (if e.throwable().isDefined then throwable(e.throwable().get()) else null)
        )

    def suiteEnd(runner: Int, task: Int, suite: String, threw: Option[Throwable]): Unit =
        emit(
          "suite-end",
          "runner" -> runner,
          "task" -> task,
          "suite" -> suite,
          "threw" -> threw.map(throwable).orNull
        )

    def runEnd(runner: Int, summary: String): Unit =
        emit("run-end", "runner" -> runner, "summary" -> summary)

    private def emit(kind: String, fields: (String, Any)*): Unit =
        out.foreach(w => write(w, kind, fields*))

    // One lock for the file: suites run in parallel, and ScalaCheck's properties on a pool.
    private def write(w: Writer, kind: String, fields: (String, Any)*): Unit = {
        val line = json(Seq("type" -> kind, "time" -> System.currentTimeMillis()) ++ fields)
        synchronized {
            w.write(line)
            w.write('\n')
            w.flush()
        }
    }

    private def prop(name: String): String = Option(System.getProperty(name)).getOrElse("unknown")

    private def versionOf(c: Class[?]): String =
        Option(c.getPackage).flatMap(p => Option(p.getImplementationVersion)).getOrElse("unknown")

    private def selector(s: Selector): Map[String, String] = s match {
        case t: TestSelector => Map("kind" -> "test", "test" -> t.testName())
        case t: NestedTestSelector =>
            Map("kind" -> "nested-test", "suite" -> t.suiteId(), "test" -> t.testName())
        case _: SuiteSelector        => Map("kind" -> "suite")
        case t: NestedSuiteSelector  => Map("kind" -> "nested-suite", "suite" -> t.suiteId())
        case t: TestWildcardSelector => Map("kind" -> "wildcard", "test" -> t.testWildcard())
        case other                   => Map("kind" -> "other", "text" -> String.valueOf(other))
    }

    private def throwable(t: Throwable): Map[String, String] = {
        val trace = new StringWriter
        t.printStackTrace(new PrintWriter(trace))
        Map(
          "class" -> t.getClass.getName,
          "message" -> capped(String.valueOf(t.getMessage), MessageChars),
          "trace" -> capped(trace.toString, TraceChars)
        )
    }

    private def capped(s: String, limit: Int): String =
        if s.length <= limit then s else s.take(limit) + s"... (${s.length - limit} more chars)"

    private def json(value: Any): String = value match {
        case null            => "null"
        case s: String       => quoted(s)
        case n: (Int | Long) => n.toString
        case b: Boolean      => b.toString
        case m: Map[?, ?]    => json(m.toSeq)
        case fields: Seq[?] =>
            fields
                .map { case (k, v) => quoted(String.valueOf(k)) + ":" + json(v) }
                .mkString("{", ",", "}")
        case other => quoted(String.valueOf(other))
    }

    // Frameworks colour their output for the terminal; the format holds plain text.
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

/** A test framework that records its events with [[CiTestEvents]] and otherwise delegates to
  * `underlying`: its fingerprints, arguments and results are the underlying framework's own.
  * `implementation` is the third-party framework class whose name and version the events report,
  * when `underlying` is itself a wrapper.
  */
abstract class RecordingFramework(underlying: Framework, implementation: Class[?])
    extends Framework {
    def this(underlying: Framework) = this(underlying, underlying.getClass)

    def name(): String = underlying.name()
    def fingerprints(): Array[Fingerprint] = underlying.fingerprints()

    def runner(args: Array[String], remoteArgs: Array[String], loader: ClassLoader): Runner = {
        val delegate = underlying.runner(args, remoteArgs, loader)
        if !CiTestEvents.enabled then delegate
        else {
            val id = CiTestEvents.runnerStart(underlying, implementation)
            new Runner {
                def args(): Array[String] = delegate.args()
                def remoteArgs(): Array[String] = delegate.remoteArgs()
                def tasks(taskDefs: Array[TaskDef]): Array[Task] =
                    delegate.tasks(taskDefs).map(recorded(id, _))
                def done(): String = {
                    val summary = delegate.done()
                    CiTestEvents.runEnd(id, summary)
                    summary
                }
            }
        }
    }

    private def recorded(runner: Int, task: Task): Task = new Task {
        def taskDef(): TaskDef = task.taskDef()
        def tags(): Array[String] = task.tags()

        def execute(handler: EventHandler, loggers: Array[Logger]): Array[Task] = {
            val suite = task.taskDef().fullyQualifiedName()
            val id = CiTestEvents.suiteStart(runner, suite)
            val recording: EventHandler = e => {
                CiTestEvents.event(runner, id, e)
                handler.handle(e)
            }
            try {
                val next = task.execute(recording, loggers).map(recorded(runner, _))
                CiTestEvents.suiteEnd(runner, id, suite, None)
                next
            } catch {
                case t: Throwable =>
                    CiTestEvents.suiteEnd(runner, id, suite, Some(t))
                    throw t
            }
        }
    }
}

/** ScalaTest, recorded. Registered in place of ScalaTest's own framework (see build.sbt). */
final class ScalaTestFrameworkRecorded extends RecordingFramework(new org.scalatest.tools.Framework)
