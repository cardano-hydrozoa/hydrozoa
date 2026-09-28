import sbt.*
import sbt.Keys.*
import sbt.internal.inc.{Analysis, Stamper}

/** A check that each compiled configuration's classes are of the sources in the working tree.
  *
  * After compiling, zinc's analysis (its record of which sources the class files were compiled
  * from) must list exactly the configuration's current sources, each with the content hash of the
  * file on disk, and every class file it names must exist. A compile that was skipped, or served
  * from a compilation of other sources, fails it. CI runs it after restoring `target/` from a build
  * of main (`.github/workflows/ci.yml`). It cannot see a class that zinc kept although a dependency
  * changed under it: only a build from scratch rules that out.
  */
object CompileState {

    val checkCompileState: TaskKey[Unit] = taskKey[Unit](
      "Compile, then fail unless zinc's analysis records exactly the current sources, by content hash."
    )

    private val compileStateProblems = taskKey[(Int, Seq[String])](
      "Compiles this configuration; its number of sources and how its analysis differs from them."
    )

    /** Main and test compile checks for a project; `settings` picks the projects that are checked. */
    val configSettings: Seq[Setting[?]] = Seq(Compile, Test).flatMap { config =>
        Seq(
          config / compileStateProblems := Def.uncached {
              // One evaluation of `sources` feeds both the compile and the comparison, so a source
              // generated on every evaluation (BuildInfo's build time) is compared as compiled.
              val analysis = (config / compile).value
              val current = (config / sources).value
              val converter = fileConverter.value
              val label = s"${thisProject.value.id}/${config.name}"
              analysis match {
                  case a: Analysis => (current.size, problems(label, a, current, converter))
                  case other =>
                      (current.size, Seq(s"$label: unexpected analysis type ${other.getClass.getName}"))
              }
          }
        )
    }

    /** The build-wide check over `projects`, main and test sources. */
    def settings(projects: ScopeFilter.ProjectFilter): Seq[Setting[?]] = Seq(
      checkCompileState := Def.uncached {
          val results =
              compileStateProblems.all(ScopeFilter(projects, inConfigurations(Compile, Test))).value
          val found = results.flatMap(_._2)
          val log = streams.value.log
          if (found.nonEmpty) {
              throw new MessageOnlyException(
                "The compiled classes are not of the current sources:\n" +
                  found.map("  " + _).mkString("\n")
              )
          }
          log.info(
            s"checkCompileState: the analyses of ${results.size} configurations record exactly " +
              s"their ${results.map(_._1).sum} current sources."
          )
      }
    )

    private def problems(
        label: String,
        analysis: Analysis,
        current: Seq[File],
        converter: xsbti.FileConverter
    ): Seq[String] = {
        val recorded = analysis.stamps.sources.map((ref, stamp) => ref.id -> stamp)
        val onDisk = current.map(f => converter.toVirtualFile(f.toPath).id -> f.toPath).toMap
        val missing = (onDisk.keySet -- recorded.keySet).toSeq.sorted
            .map(id => s"$label: $id is not in the analysis (never compiled)")
        val extra = (recorded.keySet -- onDisk.keySet).toSeq.sorted
            .map(id => s"$label: $id is in the analysis but not among the sources")
        // sbt stamps sources with zinc's farm hash. Were it to use another stamp, every source
        // would differ here: the check fails loudly rather than passing.
        val changed = onDisk.toSeq.sortBy(_._1).collect {
            case (id, path) if recorded.get(id).exists { stamp =>
                    stamp.writeStamp != Stamper.forFarmHashP(path).writeStamp
                } =>
                s"$label: $id differs from the version compiled"
        }
        val lostClasses = analysis.relations.allProducts.toSeq
            .filterNot(p => converter.toPath(p).toFile.exists)
            .map(_.id)
            .sorted
            .map(id => s"$label: class file $id is missing")
        missing ++ extra ++ changed ++ lostClasses
    }
}
