package hydrozoa.build

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets
import sbt.*

/** Keeps SLF4J's "no binding" notice out of every sbt run.
  *
  * sbt 2.0.1 ships `slf4j-api` 1.7.28 on its own boot classpath with no binding next to it, and
  * sbt-scalafix 0.14.7 loads jgit 5.13 while sbt evaluates the build's settings; jgit's `FS` has
  * a static SLF4J logger, so SLF4J initialises and prints three `SLF4J:` lines to stderr on every
  * sbt invocation. A binding (`slf4j-nop`) in `project/plugins.sbt` does not help: the plugins'
  * classloader asks sbt's boot loader first, so `LoggerFactory` comes from the boot jar, and that
  * loader cannot see a binding on the plugins' classpath.
  *
  * So this plugin initialises SLF4J itself, when sbt loads it (before any settings are evaluated),
  * with stderr captured: it drops SLF4J's own `SLF4J:` notice lines and replays everything else.
  * SLF4J then stays on its no-op fallback, exactly as it did before, only silently. Reflection keeps
  * this a no-op if SLF4J ever leaves sbt's classpath.
  *
  * Remove this file once sbt ships a binding with its `slf4j-api`, or sbt-scalafix stops loading
  * jgit at load time: then `sbt` in an empty directory with sbt-scalafix enabled prints no `SLF4J:`
  * lines without it.
  */
object QuietSlf4j extends AutoPlugin {
    override def trigger: PluginTrigger = allRequirements

    // An object's body runs once, when sbt loads the plugin.
    initialise()

    private def initialise(): Unit = {
        val original = System.err
        val captured = new ByteArrayOutputStream()
        System.setErr(new PrintStream(captured, true, StandardCharsets.UTF_8))
        try {
            val _ = Class
                .forName("org.slf4j.LoggerFactory", true, getClass.getClassLoader)
                .getMethod("getILoggerFactory")
                .invoke(null)
        } catch {
            case _: ReflectiveOperationException | _: LinkageError => ()
        } finally System.setErr(original)
        captured
            .toString(StandardCharsets.UTF_8)
            .linesIterator
            .filterNot(_.startsWith("SLF4J:"))
            .foreach(original.println)
    }
}
