package hydrozoa.bootstrap

import cats.effect.unsafe.implicits.global
import hydrozoa.config.head.network.CardanoNetwork
import java.nio.file.{Files, Path}
import org.scalatest.funsuite.AnyFunSuite

/** `build-head-config --l2-ledger any-remote` reads the remote ledger's opening state from the
  * bootstrap directory: the evacuation map and the digests the head config and the init tx must
  * carry (`l2ParamsHash`, `initialL2StateHash`). Without the digests an any-remote head would
  * certify placeholder zeros and then refuse to boot against the ledger's real ones (GUM-342).
  */
class RemoteLedgerOpeningTest extends AnyFunSuite {

    private val network = CardanoNetwork.Preview

    /** The cross-repo golden from `EvacuationMapDigestTest`: a map and its digest. */
    private val mapJson =
        """{
          |  "ab00000000000000000000000000000000000000000000000000000000000000":
          |    "a2005839000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011a00989680"
          |}""".stripMargin
    private val mapDigest = "6a0bf68bee4d771bd969fce92285d7e3da0683d01e4c2c44310e5a162ba84995"
    private val params = "11" * 32
    private val state = "22" * 32

    private def bootstrapDir(digests: Option[String], map: Option[String] = Some(mapJson)): Path = {
        val dir = Files.createTempDirectory("remote-ledger-opening")
        map.foreach(m =>
            Files.writeString(dir.resolve(Bootstrap.BootstrapDir.initialEvacuationMap), m)
        )
        digests.foreach(d =>
            Files.writeString(dir.resolve(Bootstrap.BootstrapDir.remoteLedgerDigests), d)
        )
        dir
    }

    private def read(dir: Path) =
        BuildHeadConfig.readRemoteLedgerOpening(dir, network).attempt.unsafeRunSync()

    test("reads the digests and the map, ignoring extra fields") {
        val dir = bootstrapDir(
          Some(
            s"""{"l2ParamsHash":"$params","initialL2StateHash":"$state",
               |"initialEvacuationMapHash":"$mapDigest","minAccountLovelace":1702450}""".stripMargin
          )
        )
        val opening = read(dir).fold(e => fail(e), identity)
        assert(
          opening.l2ParamsHash.toHex == params &&
              opening.initialL2StateHash.toHex == state &&
              opening.initialEvacuationMap.size == 1
        )
    }

    test("the map's digest is optional") {
        val dir =
            bootstrapDir(Some(s"""{"l2ParamsHash":"$params","initialL2StateHash":"$state"}"""))
        assert(read(dir).isRight)
    }

    test("a map digest that disagrees with the map fails the build") {
        val dir = bootstrapDir(
          Some(
            s"""{"l2ParamsHash":"$params","initialL2StateHash":"$state",
               |"initialEvacuationMapHash":"${"33" * 32}"}""".stripMargin
          )
        )
        val err = read(dir).swap.getOrElse(fail("expected a failure"))
        assert(err.getMessage.contains("different ledger states"), err.getMessage)
    }

    test("a missing digests file fails the build and names the file") {
        val err = read(bootstrapDir(None)).swap.getOrElse(fail("expected a failure"))
        assert(err.getMessage.contains(Bootstrap.BootstrapDir.remoteLedgerDigests), err.getMessage)
    }

    test("a missing map still fails the build, as before") {
        val dir = bootstrapDir(
          Some(s"""{"l2ParamsHash":"$params","initialL2StateHash":"$state"}"""),
          map = None
        )
        val err = read(dir).swap.getOrElse(fail("expected a failure"))
        assert(err.getMessage.contains(Bootstrap.BootstrapDir.initialEvacuationMap), err.getMessage)
    }

    test("digests must be 32 bytes of hex") {
        for bad <- List("abcd", "zz" * 32) do {
            val dir =
                bootstrapDir(Some(s"""{"l2ParamsHash":"$bad","initialL2StateHash":"$state"}"""))
            assert(read(dir).isLeft, bad)
        }
    }
}
