package hydrozoa.config.loader

import cats.effect.*
import cats.effect.unsafe.implicits.global
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.loader.Codecs.given
import hydrozoa.config.node.MultiNodeConfig
import io.circe.syntax.*
import io.circe.{Json, *}
import org.scalacheck.rng.Seed
import org.scalacheck.{Properties, Test}

object LoaderTest extends Properties("Configuration Loader Properties") {
    import MultiNodeConfig.*

    override def overrideParameters(p: Test.Parameters): Test.Parameters =
            p.withInitialSeed(Seed.fromBase64("AIUK99d5Zgz2qjvWyA8NCvvkh9Q5mbFWbux4o6hfQZG=").get)

    val headConfigRoundTrip: MultiNodeConfigTestM[Boolean] =
        for {
            mnc <- ask
            headConfig = mnc.headConfig
            encoded = headConfig.asJson
            decoded <- failLeft(encoded.as[HeadConfig])
            _ <- assertWith(
              headConfig == decoded,
              "HeadConfig should round trip through JSON." +
                  "=" * 80 + s"\nMarshalled:\n\n ${headConfig} \n\n" +
                  "=" * 80 + s"\nEncoded:\n\n ${encoded} \n\n" +
                  "=" * 80 + s"\nDecoded:\n\n ${decoded} \n\n"
            )
        } yield true

    val dumpHeadConfig: MultiNodeConfigTestM[Unit] =
        for {
            mnc <- ask
            _ <- lift(IO.println(mnc.headConfig.asJson))

        } yield ()

    val _ = property("round tripping") = runDefault(
      for {
          _ <- dumpHeadConfig
          _ <- headConfigRoundTrip
      } yield true
    )
}
