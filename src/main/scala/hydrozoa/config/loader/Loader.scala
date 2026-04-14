package hydrozoa.config.loader

import cats.effect.*
import cats.effect.unsafe.implicits.global
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import io.circe.*
import scala.io.{BufferedSource, Source}
import scalus.crypto.ed25519.VerificationKey

object Loader {
    def file(name: String = "config.toml"): Resource[IO, BufferedSource] =
        Resource.fromAutoCloseable(IO(Source.fromFile(name)))

    def main(args: Array[String]): Unit =
        file()
            .use(bufferedSource =>
                val raw: String = bufferedSource.getLines().mkString("\n")
                val eParsed = Json.fromString(raw).as[VerificationKey]
                for {
                    parsed <- IO.fromEither(eParsed)
                    _ <- IO.println(parsed)
                } yield ()
            )
            .unsafeRunSync()
}
