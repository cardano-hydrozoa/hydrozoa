import java.io.File
import java.net.URLClassLoader
import sbt.*
import sbt.Keys.*

/** The lint rule against bare throwables: no class compiled from this build's sources may extend
  * `Throwable` other than through `Exception`.
  *
  * A value that is a `Throwable` but not an `Exception` (one extending `Throwable` or `Error`
  * directly) is one that libraries mishandle. cats-actors is the case we met: its user guardian's
  * rules cover only `Exception`s, and a bare `Throwable` that reached it left a live JVM with a dead
  * actor system. Scala has no checked exceptions to make such a type stand out, so this check does.
  *
  * It reads the compiled classes rather than the source, so it sees every way a class can get
  * there: `extends Throwable`, `with Throwable`, an `enum`, a subclass of another class of ours, or
  * of a library's non-`Exception` throwable such as `ControlThrowable`. Throwing an instance of a
  * library's `Error` (`assert`, `???`) is not a class of ours, and is not checked.
  */
object NoBareThrowables {

    val checkNoBareThrowables: TaskKey[Unit] = taskKey[Unit](
      "Fail if a class compiled from this build's sources extends Throwable other than through Exception."
    )

    /** The check, over the compiled classes of `projects`, main and test. */
    def settings(projects: ScopeFilter): Seq[Setting[?]] = Seq(
      checkNoBareThrowables := Def.uncached {
          val converter = fileConverter.value
          val ids = thisProject.all(projects).value.map(_.id)
          val mainDirs = (Compile / classDirectory).all(projects).value
          val testDirs = (Test / classDirectory).all(projects).value
          val classpaths = (Test / fullClasspath).all(projects).value
          val findings = ids.indices.map { i =>
              inspect(
                Seq(mainDirs(i), testDirs(i)),
                classpaths(i).map(a => converter.toPath(a.data).toFile)
              )
          }
          val bare = findings.flatMap(_._1).distinct.sorted
          val unreadable = findings.flatMap(_._2).distinct.sortBy(_._1)
          if (unreadable.nonEmpty) {
              throw new MessageOnlyException(
                "checkNoBareThrowables could not load these classes, so it cannot vouch for " +
                  "them:\n" + unreadable.map((n, e) => s"  $n: $e").mkString("\n")
              )
          }
          if (bare.nonEmpty) {
              throw new MessageOnlyException(
                "These classes extend Throwable other than through Exception:\n" +
                  bare.map("  " + _).mkString("\n") +
                  "\nRoot them at Exception (usually RuntimeException), or at a more specific " +
                  "subclass. See project/NoBareThrowables.scala for why."
              )
          }
      }
    )

    /** The bare throwables among the classes in `classDirs`, and the classes that could not be
      * loaded (with why), loading each without initialising it.
      */
    def inspect(classDirs: Seq[File], classpath: Seq[File]): (Seq[String], Seq[(String, String)]) = {
        val loader =
            new URLClassLoader(classpath.map(_.toURI.toURL).toArray, ClassLoader.getPlatformClassLoader)
        try {
            val names = classDirs.filter(_.isDirectory).flatMap { dir =>
                (dir ** "*.class").get().flatMap(f => IO.relativize(dir, f)).collect {
                    case rel if !rel.endsWith("module-info.class") =>
                        rel.stripSuffix(".class").replace(File.separatorChar, '.')
                }
            }
            val results = names.map { name =>
                try {
                    val c = Class.forName(name, false, loader)
                    val isBare =
                        classOf[Throwable].isAssignableFrom(c) &&
                            !classOf[Exception].isAssignableFrom(c)
                    Left(Option.when(isBare)(name))
                } catch {
                    case e: LinkageError           => Right(name -> e.toString)
                    case e: ClassNotFoundException => Right(name -> e.toString)
                }
            }
            (results.collect { case Left(Some(n)) => n }, results.collect { case Right(u) => u })
        } finally loader.close()
    }
}
