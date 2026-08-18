package hydrozoa.app.cli

import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import com.monovore.decline.{Command, Opts}
import java.nio.file.{Files, Path}

/** The `scaffold` subcommand: materialize the operator-facing workspace files that are baked into
  * the image, so a Docker-only user never has to clone the repo. Into `<dir>` (default `.`) it
  * writes `docker-compose.yml` (durable stores + an `evacuate` profile built in), `hydrozoa.sh`
  * (the CLI alias — `source` it), and `template/peer-private.template.json.local` (fill in
  * `blockfrostApiKey`) — the layout every later command expects, so they run with no path flags.
  * The scaffold dir itself is the head directory (the default `$HYDROZOA_HOME` that `hydrozoa.sh`
  * sets), so the whole workspace is self-contained. Refuses to overwrite existing files.
  */
object Scaffold:

    /** (classpath resource baked by the build, destination relative to the scaffold dir). */
    private val resources: List[(String, String)] = List(
      "/scaffold/docker-compose.yml" -> "docker-compose.yml",
      "/scaffold/hydrozoa.sh" -> "hydrozoa.sh",
      "/scaffold/peer-private.template.json" -> "template/peer-private.template.json.local"
    )

    private val dirArg: Opts[Path] =
        Opts.argument[String]("dir").map(Path.of(_)).withDefault(Path.of("."))

    /** The `scaffold` subcommand. */
    lazy val command: Command[IO[ExitCode]] =
        Command(
          name = "scaffold",
          header = "Write the Docker workspace files (docker-compose.yml, hydrozoa.sh, config " +
              "template) into a directory — no repo clone needed"
        )(dirArg.map(run))

    private def run(dir: Path): IO[ExitCode] =
        for {
            // Skip files that already exist (never clobber a filled-in template, and let a repo
            // checkout that already has docker-compose.yml / hydrozoa.sh materialize only the rest).
            _ <- resources.traverse_ { (res, rel) =>
                val dst = dir.resolve(rel)
                IO.blocking(Files.exists(dst)).flatMap {
                    case true => IO.println(s"skipped $dst (exists)")
                    case false =>
                        IO.blocking {
                            Option(dst.getParent).foreach(Files.createDirectories(_))
                            val in = Option(getClass.getResourceAsStream(res)).getOrElse(
                              throw new IllegalStateException(s"missing baked resource $res")
                            )
                            try Files.copy(in, dst)
                            finally in.close()
                        } *> IO.println(s"wrote $dst")
                }
            }
            _ <- IO.println(
              s"Scaffolded into $dir. Next: set blockfrostApiKey in " +
                  "template/peer-private.template.json.local, then `source ./hydrozoa.sh`."
            )
        } yield ExitCode.Success

end Scaffold
